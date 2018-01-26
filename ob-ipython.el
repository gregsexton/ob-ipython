;;; ob-ipython.el --- org-babel functions for IPython evaluation

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Keywords: literate programming, reproducible research
;; Homepage: http://www.gregsexton.org
;; Package-Requires: ((s "1.9.0") (dash "2.10.0") (dash-functional "1.2.0") (f "0.17.2") (emacs "24"))

;; The MIT License (MIT)

;; Copyright (c) 2015 Greg Sexton

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; Org-Babel support for evaluating Python source code using IPython.

;;; Code:

(require 'ob)
(require 'ob-python)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'f)
(require 'json)
(require 'python)
(require 'cl)

;; variables

(defcustom ob-ipython-kernel-extra-args '()
  "List of extra args to pass when creating a kernel."
  :group 'ob-ipython)

(defcustom ob-ipython-client-path
  (f-expand "./client.py"
            (or (-when-let (f load-file-name) (f-dirname f)) default-directory))
  "Path to the client script."
  :group 'ob-ipython)

(defcustom ob-ipython-command
  "jupyter"
  "Command to launch ipython. Usually ipython or jupyter."
  :group 'ob-ipython)

(defcustom ob-ipython-resources-dir "./obipy-resources/"
  "Directory where resources (e.g images) are stored so that they
can be displayed.")

(defcustom ob-ipython-kernel-paths
  `("./" ,(replace-regexp-in-string "\n$" ""
                                    (shell-command-to-string
                                     (s-concat ob-ipython-command " --runtime-dir"))))
  "Directories where to look for ipython/jupyter kernels.")


;; utils

(defun ob-ipython--write-string-to-file (file string)
  (if string
      (with-temp-buffer
        (let ((require-final-newline nil))
          (insert string)
          (write-file file)))
    (error "No output was produced to write to a file.")))

(defun ob-ipython--write-base64-string (file b64-string)
  (if b64-string
      (with-temp-buffer
        (let ((buffer-file-coding-system 'binary)
              (require-final-newline nil))
          (insert b64-string)
          (base64-decode-region (point-min) (point-max))
          (write-file file)))
    (error "No output was produced to write to a file.")))

(defun ob-ipython--create-traceback-buffer (traceback)
  (let ((buf (get-buffer-create "*ob-ipython-traceback*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (-each traceback
          (lambda (line) (insert (format "%s\n" line))))
        (ansi-color-apply-on-region (point-min) (point-max))))
    (pop-to-buffer buf)))

(defun ob-ipython--create-inspect-buffer (doc)
  (let ((buf (get-buffer-create "*ob-ipython-inspect*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert doc)
        (ansi-color-apply-on-region (point-min) (point-max))
        (whitespace-cleanup)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun ob-ipython--clear-output-buffer ()
  (let ((buf (get-buffer-create "*ob-ipython-out*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun ob-ipython--output (output append-p)
  (when (not (s-blank? output))
    (let ((buf (get-buffer-create "*ob-ipython-out*")))
      (with-current-buffer buf
        (special-mode)
        (let ((inhibit-read-only t))
          (unless append-p (erase-buffer))
          (when (s-blank? (buffer-string)) (pop-to-buffer buf))
          (let ((p (point)))
            (if (= p (point-max))     ;allow tailing
                (progn (insert output)
                       (-when-let (w (get-buffer-window buf 'visible))
                         (set-window-point w (point-max))))
              (save-excursion
                (goto-char (point-max))
                (insert output)))
            (ansi-color-apply-on-region p (point-max))
            ;; this adds some support for control chars
            (comint-carriage-motion p (point-max)))
          (unless append-p (goto-char (point-min))))))))

(defun ob-ipython--dump-error (err-msg)
  (with-current-buffer (get-buffer-create "*ob-ipython-debug*")
    (special-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert err-msg)
      (goto-char (point-min))))
  (error "There was a fatal error trying to process the request. See *ob-ipython-debug*"))

(defun ob-ipython--generate-file-name (suffix)
  (s-concat (make-temp-name ob-ipython-resources-dir) suffix))

;; process management

(defun ob-ipython--kernel-file (name kernel)
  (if (s-ends-with-p ".json" name)
      name
    (format "emacs-%s-%s.json" (ob-ipython--get-display-name kernel) name)))

(defun ob-ipython--kernel-repl-cmd (name kernel)
  (list ob-ipython-command "console" "--simple-prompt" "--existing"
        (ob-ipython--kernel-file name kernel)))

;;; TODO: could setup a default sentinel that outputs error on process
;;; early termination
(defun ob-ipython--create-process (kernel-name cmd)
  (let ((buf (get-buffer-create (format "*ob-ipython-%s*" kernel-name))))
    (with-current-buffer buf (erase-buffer))
    (apply 'start-process kernel-name buf (car cmd) (cdr cmd))))

(defun ob-ipython--get-python ()
  (locate-file (if (eq system-type 'windows-nt)
                   "python.exe"
                 (or python-shell-interpreter "python"))
               exec-path))

(defun ob-ipython--create-kernel (name kernel)
  (let ((language-display (ob-ipython--get-display-name kernel)))
    (when (and (not (ignore-errors (process-live-p (get-process (format "kernel-%s-%s"
                                                                        language-display name)))))
               (not (s-ends-with-p ".json" name)))
      (ob-ipython--create-process
       (format "kernel-%s-%s" language-display name)
       (append
        (list ob-ipython-command "console" "--simple-prompt")
        (list "-f" (ob-ipython--kernel-file name kernel))
        (if kernel (list "--kernel" kernel) '())
        ;;should be last in the list of args
        ob-ipython-kernel-extra-args))
      (sleep-for 1))
    )
  )

(defun ob-ipython--get-kernel-processes ()
  (let ((procs (-filter (lambda (p)
                          (s-starts-with? "kernel-" (process-name p)))
                        (process-list))))
    (-zip (-map (-compose (-partial 's-replace "kernel-" "")
                          'process-name)
                procs)
          procs)))

(defun ob-ipython--create-repl (name kernel)
  (let* ((kernel (if (s-ends-with-p ".json" name)
                     (ob-ipython--get-kernel-from-file name)
                   kernel))
         (language-display (ob-ipython--get-display-name kernel))
         (process-name (format "Jupyter:%s:%s" language-display name))
         (cmd (s-join " " (ob-ipython--kernel-repl-cmd name kernel))))
    (get-buffer-process
     (python-shell-make-comint cmd process-name nil))
    (format "*%s*" process-name)))

;; kernel management

(defun ob-ipython--choose-kernel ()
  (let ((procs (ob-ipython--get-kernel-processes)))
    (-> (ido-completing-read "kernel? " (-map 'car procs) nil t)
        (assoc procs)
        cdr
        list)))

;;; TODO: broken
(defun ob-ipython-interrupt-kernel (proc)
  "Interrupt a running kernel. Useful for terminating infinite
loops etc. If things get really desparate try `ob-ipython-kill-kernel'."
  (interactive (ob-ipython--choose-kernel))
  (when proc
    (interrupt-process proc)
    (message (format "Interrupted %s" (process-name proc)))))

(defun ob-ipython-kill-kernel (proc)
  "Kill a kernel process. If you then re-evaluate a source block
a new kernel will be started."
  (interactive (ob-ipython--choose-kernel))
  (when proc
    (delete-process proc)
    (message (format "Killed %s" (process-name proc)))))

;; evaluation

(defvar ob-ipython--async-queue nil)

(defun ob-ipython--enqueue (q x)
  (set q (append (symbol-value q) (list x))))

(defun ob-ipython--dequeue (q)
  (let ((ret (car (symbol-value q))))
    (set q (cdr (symbol-value q)))
    ret))

(defun ob-ipython--collect-json ()
  ;; this function assumes that we're in a buffer with the json lines
  (let ((json-array-type 'list))
    (let (acc)
      (while (not (= (point) (point-max)))
        (setq acc (cons (json-read) acc))
        (forward-line))
      (nreverse acc))))

(defun ob-ipython--running-p ()
  (get-process "execute"))

(defun ob-ipython--run-async (code name callback args)
  (let ((proc (ob-ipython--create-process
               "execute"
               (list (ob-ipython--get-python)
                     "--" ob-ipython-client-path "--conn-file" name "--execute"))))
    ;; TODO: maybe add a way of disabling streaming output?
    ;; TODO: cleanup and break out - we parse twice, can we parse once?
    (set-process-filter
     proc
     (lexical-let ((parse-pos 0))
       (lambda (proc output)
         ;; not guaranteed to be given lines - we need to handle buffering
         (with-current-buffer (process-buffer proc)
           (goto-char (point-max))
           (insert output)
           (let ((json-array-type 'list))
             (goto-char parse-pos)
             (while (not (= (point) (point-max)))
               (condition-case nil
                   (progn (-> (json-read)
                              list
                              ob-ipython--extract-output
                              (ob-ipython--output t))
                          (forward-line)
                          (setq parse-pos (point)))
                 (error (goto-char (point-max))))))))))
    (set-process-sentinel
     proc
     (lexical-let ((callback callback)
                   (args args))
       (lambda (proc state)
         (when (not (process-live-p proc))
           (with-current-buffer (process-buffer proc)
             (goto-char (point-min))
             (apply callback (-> (ob-ipython--collect-json)
                                 ob-ipython--eval
                                 (cons args))))
           (ob-ipython--maybe-run-async)))))
    (process-send-string proc code)
    (process-send-string proc "\n")
    (process-send-eof proc)))

(defun ob-ipython--maybe-run-async ()
  (when (not (ob-ipython--running-p))
    (when-let (val (ob-ipython--dequeue 'ob-ipython--async-queue))
      (cl-destructuring-bind (code name callback args) val
        (ob-ipython--run-async code name callback args)))))

(defun ob-ipython--execute-request-async (code kernel-name callback args)
  (ob-ipython--enqueue 'ob-ipython--async-queue (list code kernel-name callback args))
  (ob-ipython--maybe-run-async))

(defun ob-ipython--execute-request (code kernel-name)
  (with-temp-buffer
    (let ((ret (apply 'call-process-region code nil
                      (ob-ipython--get-python) nil t nil
                      (list "--" ob-ipython-client-path "--conn-file" kernel-name "--execute"))))
      (if (> ret 0)
          (ob-ipython--dump-error (buffer-string))
        (goto-char (point-min))
        (ob-ipython--collect-json)))))

(defun ob-ipython--extract-output (msgs)
  (->> msgs
       (-filter (lambda (msg) (string= "stream" (cdr (assoc 'msg_type msg)))))
       (-filter (lambda (msg) (-contains? '("stdout" "stderr")
                                          (->> msg (assoc 'content)
                                               (assoc 'name)
                                               cdr))))
       (-map (lambda (msg) (->> msg (assoc 'content) (assoc 'text) cdr)))
       (-reduce 's-concat)))

(defun ob-ipython--extract-result (msgs)
  `((:value . ,(->> msgs
                    (-filter (lambda (msg)
                               (s-equals? "execute_result"
                                          (cdr (assoc 'msg_type msg)))))
                    (-mapcat (lambda (msg)
                               (->> msg (assoc 'content) (assoc 'data) cdr)))))
    (:display . ,(->> msgs
                      (-filter (lambda (msg)
                                 (s-equals? "display_data"
                                            (cdr (assoc 'msg_type msg)))))
                      (-mapcat (lambda (msg)
                                 (->> msg (assoc 'content) (assoc 'data) cdr)))))))

(defun ob-ipython--extract-error (msgs)
  (let ((error-content
         (->> msgs
              (-filter (lambda (msg) (-contains? '("execute_reply" "inspect_reply")
                                                 (cdr (assoc 'msg_type msg)))))
              car
              (assoc 'content)
              cdr)))
    ;; TODO: this doesn't belong in this abstraction
    (ob-ipython--create-traceback-buffer (cdr (assoc 'traceback error-content)))
    (format "%s: %s" (cdr (assoc 'ename error-content)) (cdr (assoc 'evalue error-content)))))

(defun ob-ipython--extract-status (msgs)
  (->> msgs
       (-filter (lambda (msg) (-contains? '("execute_reply" "inspect_reply" "complete_reply")
                                          (cdr (assoc 'msg_type msg)))))
       car
       (assoc 'content)
       (assoc 'status)
       cdr))

(defun ob-ipython--extract-execution-count (msgs)
  (->> msgs
       (-filter (lambda (msg) (-contains? '("execute_reply")
                                          (cdr (assoc 'msg_type msg)))))
       car
       (assoc 'content)
       (assoc 'execution_count)
       cdr))

(defun ob-ipython--eval (service-response)
  (let ((status (ob-ipython--extract-status service-response)))
    (cond ((string= "ok" status) `((:result . ,(ob-ipython--extract-result service-response))
                                   (:output . ,(ob-ipython--extract-output service-response))
                                   (:exec-count . ,(ob-ipython--extract-execution-count service-response))))
          ((string= "abort" status) (error "Kernel execution aborted."))
          ((string= "error" status) (error (ob-ipython--extract-error service-response))))))

;; inspection

(defun ob-ipython--inspect-request (code &optional pos detail)
  (let ((input (json-encode `((code . ,code)
                              (pos . ,(or pos (length code)))
                              (detail . ,(or detail 0)))))
        (args (list "--" ob-ipython-client-path
                    "--conn-file"
                    (ob-ipython--get-session-from-edit-buffer (current-buffer))
                    "--inspect")))
    (with-temp-buffer
      (let ((ret (apply 'call-process-region input nil
                        (ob-ipython--get-python) nil t nil
                        args)))
        (if (> ret 0)
            (ob-ipython--dump-error (buffer-string))
          (goto-char (point-min))
          (ob-ipython--collect-json))))))

(defun ob-ipython--inspect (code pos)
  "Given a piece of code and a point position, return inspection results."
  (let* ((resp (ob-ipython--inspect-request code pos 0))
         (status (ob-ipython--extract-status resp)))
    (if (string= "ok" status)
        (->> resp
             (-filter (lambda (msg)
                        (-contains? '("execute_result" "display_data" "inspect_reply")
                                    (cdr (assoc 'msg_type msg)))))
             (-mapcat (lambda (msg)
                        (->> msg
                             (assoc 'content)
                             (assoc 'data)
                             cdr))))
      (error (ob-ipython--extract-error resp)))))

(defun ob-ipython-inspect (buffer pos)
  "Ask a kernel for documentation on the thing at POS in BUFFER."
  (interactive (list (current-buffer) (point)))
  (let ((code (with-current-buffer buffer
                (buffer-substring-no-properties (point-min) (point-max)))))
    (-if-let (result (->> (ob-ipython--inspect code pos)
                          (assoc 'text/plain)
                          cdr))
        (ob-ipython--create-inspect-buffer result)
      (message "No documentation was found."))))

;; completion

(defun ob-ipython--complete-request (code &optional pos)
  (let ((input (json-encode `((code . ,code)
                              (pos . ,(or pos (length code))))))
        (args (list "--" ob-ipython-client-path "--conn-file"
                    (ob-ipython--get-session-from-edit-buffer (current-buffer))
                    "--complete")))
    (with-temp-buffer
      (let ((ret (apply 'call-process-region input nil
                        (ob-ipython--get-python) nil t nil
                        args)))
        (if (> ret 0)
            (ob-ipython--dump-error (buffer-string))
          (goto-char (point-min))
          (ob-ipython--collect-json))))))

(defun ob-ipython-completions (buffer pos)
  "Ask a kernel for completions on the thing at POS in BUFFER."
  (let* ((code (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
         (resp (ob-ipython--complete-request code pos))
         (status (ob-ipython--extract-status resp)))
    (if (not (string= "ok" status))
        '()
      (->> resp
           (-filter (lambda (msg)
                      (-contains? '("complete_reply")
                                  (cdr (assoc 'msg_type msg)))))
           (-mapcat (lambda (msg)
                      (->> msg
                           (assoc 'content)
                           cdr)))))))

(defun ob-ipython--company-doc-buffer (doc)
  "Make company-suggested doc-buffer with ansi-color support."
  (let ((buf (company-doc-buffer doc)))
    (with-current-buffer buf
      (ansi-color-apply-on-region (point-min) (point-max)))
    buf))

(defun company-ob-ipython (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ob-ipython))
    (prefix (and ob-ipython-mode
                 (let ((res (ob-ipython-completions (current-buffer) (1- (point)))))
                   (substring-no-properties (buffer-string)
                                            (cdr (assoc 'cursor_start res))
                                            (cdr (assoc 'cursor_end res))))))
    (candidates (cons :async (lambda (cb)
                               (let ((res (ob-ipython-completions
                                           (current-buffer) (1- (point)))))
                                 (funcall cb (cdr (assoc 'matches res)))))))
    (sorted t)
    (doc-buffer (ob-ipython--company-doc-buffer
                 (cdr (assoc 'text/plain (ob-ipython--inspect arg (length arg))))))))

;; mode

(define-minor-mode ob-ipython-mode
  ""
  nil
  " ipy"
  '())

;; babel framework

(add-to-list 'org-src-lang-modes '("ipython" . python))
(add-hook 'org-mode-hook 'ob-ipython-auto-configure-kernels)

(defvar ob-ipython-configured-kernels nil)

(defun ob-ipython--get-kernel-from-file (name)
  "Return kernel from provided filename.  If kernel
is empty, return python by default."
  ;; TODO Should we be more clever about the default python kernel returned?
  (let* ((filename (->> (-map (lambda (dir) (directory-files dir t name))
                              ob-ipython-kernel-paths)
                        (-flatten)
                        (car)))
         (kernel (if filename
                     (->> filename
                          (json-read-file)
                          (assoc 'kernel_name)
                          (cdr))
                   (error "Can't find kernel file; make sure jupyter paths are correct and kernel file is there"))))

    (if (string= "" kernel)
        "python"
      kernel)))

(defun ob-ipython--get-kernels ()
  "Return a list of available jupyter kernels and their corresponding languages
and display names. The elements of the list have the form (\"kernel\" \"language\" \"display_name\")."
  (and ob-ipython-command
       (let ((kernelspecs (cdar (json-read-from-string
                                 (shell-command-to-string
                                  (s-concat ob-ipython-command " kernelspec list --json"))))))
         (->> kernelspecs
              (-map (lambda (kernelspec)
                      (list (symbol-name (car kernelspec))
                            (->> (cdr kernelspec)
                                 (assoc 'spec)
                                 cdr
                                 (assoc 'language)
                                 cdr)
                            (->> (cdr kernelspec)
                                 (assoc 'spec)
                                 cdr
                                 (assoc 'display_name)
                                 cdr
                                 (replace-regexp-in-string "[[:space:]]" "")
                                 ))))
              ;; We'll later make python without a version number the default.
              (-insert-at 0 '("python" "python" "Python"))
              ;; When invoking via jupyter-X, we want X (i.e., language) case insensitive.
              ;; Here, we insert copies of the above found kernels with lower case names.
              (-map (lambda (x) (list x (-snoc (-butlast x) (downcase (-last-item x))))))
              (-flatten-n 1)
              )
         )))

(defun ob-ipython--get-display-name (kernel)
  "Return display-name for KERNEL."
  ;; get display-name for kernel and capitalize first letter
  (or (capitalize (nth 2 (assoc kernel ob-ipython-configured-kernels)))
      (capitalize kernel)))

(defun ob-ipython--configure-kernel (kernel-lang-display)
  "Configure org mode to use specified kernel."
  (let* ((kernel (car kernel-lang-display))
         (language (nth 1 kernel-lang-display))
         (display-name (nth 2 kernel-lang-display))
         (jupyter-lang (concat "jupyter-" display-name))
         (mode (intern (or (cdr (assoc language org-src-lang-modes))
                           (replace-regexp-in-string "[0-9]*" "" language))))
         (header-args (intern (concat "org-babel-default-header-args:" jupyter-lang))))
    (add-to-list 'org-src-lang-modes `(,jupyter-lang . ,mode))
    ;; Only set defaults if the corresponding variable is nil or does not
    ;; exist yet.
    (unless (and (boundp header-args) (symbol-value header-args))
      (set (intern (concat "org-babel-default-header-args:" jupyter-lang))
           `(;;(:session . ,language)
             (:kernel . ,kernel))))
    (defalias (intern (concat "org-babel-execute:" jupyter-lang))
      'org-babel-execute:ipython)
    (defalias (intern (concat "org-babel-" jupyter-lang "-initiate-session"))
      'org-babel-ipython-initiate-session)
    kernel-lang))

(defun ob-ipython-auto-configure-kernels (&optional replace)
  "Auto-configure kernels for use with org-babel based on the
available kernelspecs of the current jupyter installation. If
REPLACE is non-nil, force configuring the kernels even if they
have previously been configured."
  (interactive (list t))
  (when (or replace (not ob-ipython-configured-kernels))
    (setq ob-ipython-configured-kernels
          (-map 'ob-ipython--configure-kernel (ob-ipython--get-kernels))))
  ;; Set default kernel to python
  (setq org-babel-default-header-args:ipython '((:kernel . "python"))))

(defvar org-babel-default-header-args:ipython '())

(defun org-babel-edit-prep:ipython (info)
  ;; TODO: based on kernel, should change the major mode
  (ob-ipython--create-kernel (->> info (nth 2) (assoc :session) cdr
                                  ob-ipython--normalize-session)
                             (->> info (nth 2) (assoc :kernel) cdr))
  (ob-ipython-mode +1))

(defun ob-ipython--normalize-session (session)
  (if (string= "default" session)
      (error "default is reserved for when no name is provided. Please use a different session name.")
    (or session "default")))

(defun ob-ipython--get-session-from-edit-buffer (buffer)
  (with-current-buffer buffer
    (let* ((params (nth 2 org-src--babel-info))
           (kernel (cdr (assoc :kernel params)))
           (session (cdr (assoc :session params))))
      (if (s-ends-with? ".json" session)
          session
        (format "%s-%s"
                (ob-ipython--get-display-name kernel)
                (ob-ipython--normalize-session session))))))

(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."
  (ob-ipython--clear-output-buffer)
  (if (cdr (assoc :async params))
      (ob-ipython--execute-async body params)
    (ob-ipython--execute-sync body params)))

(defun ob-ipython--execute-async (body params)
  (let* ((file (cdr (assoc :ipyfile params)))
         (session (cdr (assoc :session params)))
         (kernel (cdr (assoc :kernel params)))
         (result-type (cdr (assoc :result-type params)))
         (sentinel (ipython--async-gen-sentinel)))
    (ob-ipython--create-kernel (ob-ipython--normalize-session session)
                               (cdr (assoc :kernel params)))
    (ob-ipython--execute-request-async
     (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                    params (org-babel-variable-assignments:python params))
     (if (s-ends-with? ".json" session)
         session
       (format "%s-%s"
               (ob-ipython--get-display-name kernel)
               (ob-ipython--normalize-session session)))
     (lambda (ret sentinel buffer file result-type)
       (let ((replacement (ob-ipython--process-response ret file result-type)))
         (ipython--async-replace-sentinel sentinel buffer replacement)))
     (list sentinel (current-buffer) file result-type))
    (format "%s - %s" (length ob-ipython--async-queue) sentinel)))

(defun ob-ipython--execute-sync (body params)
  (let* ((file (cdr (assoc :ipyfile params)))
         (session (cdr (assoc :session params)))
         (kernel (cdr (assoc :kernel params)))
         (result-type (cdr (assoc :result-type params))))
    (ob-ipython--create-kernel (ob-ipython--normalize-session session)
                               (cdr (assoc :kernel params)))
    (-when-let (ret (ob-ipython--eval
                     (ob-ipython--execute-request
                      (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                     params (org-babel-variable-assignments:python params))
                      (if (s-ends-with? ".json" session)
                          session
                        (format "%s-%s"
                                (ob-ipython--get-display-name kernel)
                                (ob-ipython--normalize-session session))))))
      (ob-ipython--process-response ret file result-type))))

(defun ob-ipython--process-response (ret file result-type)
  (let ((result (cdr (assoc :result ret)))
        (output (cdr (assoc :output ret))))
    (if (eq result-type 'output)
        output
      (ob-ipython--output output nil)
      (s-concat
       (format "# Out[%d]:\n" (cdr (assoc :exec-count ret)))
       (s-join "\n" (->> (-map (-partial 'ob-ipython--render file)
                               (list (cdr (assoc :value result))
                                     (cdr (assoc :display result))))
                         (remove-if-not nil)))))))

(defun ob-ipython--render (file-or-nil values)
  (let ((org (lambda (value) value))
        (png (lambda (value)
               (let ((file (or file-or-nil (ob-ipython--generate-file-name ".png"))))
                 (ob-ipython--write-base64-string file value)
                 (format "[[file:%s]]" file))))
        (svg (lambda (value)
               (let ((file (or file-or-nil (ob-ipython--generate-file-name ".svg"))))
                 (ob-ipython--write-string-to-file file value)
                 (format "[[file:%s]]" file))))
        (html (lambda (value)
                ;; ((eq (car value) 'text/html)
                ;;  (let ((pandoc (executable-find "pandoc")))
                ;;    (and pandoc (with-temp-buffer
                ;;                  (insert value)
                ;;                  (shell-command-on-region
                ;;                   (point-min) (point-max)
                ;;                   (format "%s -f html -t org" pandoc) t t)
                ;;                  (s-trim (buffer-string))))))
                ))
        (txt (lambda (value)
               (let ((lines (s-lines value)))
                 (if (cdr lines)
                     (->> lines
                          (-map 's-trim)
                          (s-join "\n  ")
                          (s-concat "  ")
                          (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"))
                   (s-concat ": " (car lines)))))))
    (or (-when-let (val (cdr (assoc 'text/org values))) (funcall org val))
        (-when-let (val (cdr (assoc 'image/png values))) (funcall png val))
        (-when-let (val (cdr (assoc 'image/svg+xml values))) (funcall svg val))
        (-when-let (val (cdr (assoc 'text/plain values))) (funcall txt val)))))

(defun org-babel-prep-session:ipython (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references"
  ;; c-u c-c c-v c-z
  (error "Currently unsupported."))

(defun org-babel-load-session:ipython (session body params)
  "Load BODY into SESSION."
  ;; c-c c-v c-l
  (error "Currently unsupported."))

(defun org-babel-ipython-initiate-session (&optional session params)
  "Create a session named SESSION according to PARAMS."
  (if (string= session "none")
      (error "ob-ipython currently only supports evaluation using a session.
Make sure your src block has a :session param.")
    (when (not (s-ends-with-p ".json" session))
      (ob-ipython--create-kernel (ob-ipython--normalize-session session)
                                 (cdr (assoc :kernel params))))
    (ob-ipython--create-repl (ob-ipython--normalize-session session)
                             (cdr (assoc :kernel params)))))

;; async

(defun ipython--async-gen-sentinel ()
  ;; lifted directly from org-id. thanks.
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                          (random)
                          (current-time)
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number
                       (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

(defun ipython--async-replace-sentinel (sentinel buffer replacement)
  (save-window-excursion
    (save-excursion
      (save-restriction
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward sentinel)
          (re-search-backward "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)")
          (org-babel-remove-result)
          (org-babel-insert-result
           replacement
           (cdr (assoc :result-params (nth 2 (org-babel-get-src-block-info)))))
          (org-redisplay-inline-images))))))

;; lib

(provide 'ob-ipython)

;;; ob-ipython.el ends here
