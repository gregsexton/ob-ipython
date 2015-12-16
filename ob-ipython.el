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

;;; variables

(defcustom ob-ipython-kernel-extra-args '()
  "List of extra args to pass when creating a kernel."
  :group 'ob-ipython)

(defcustom ob-ipython-driver-port 9988
  "Port to use for http driver."
  :group 'ob-ipython)

(defcustom ob-ipython-driver-hostname "localhost"
  "Hostname to use for http driver."
  :group 'ob-ipython)

(defcustom ob-ipython-driver-path
  (f-expand "./driver.py"
            (or (-when-let (f load-file-name) (f-dirname f)) default-directory))
  "Path to the driver script."
  :group 'ob-ipython)

(defcustom ob-ipython-command
  "jupyter"
  "Command to launch ipython. Usually ipython or jupyter."
  :group 'ob-ipython)

;;; utils

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

(defun ob-ipython--create-stdout-buffer (stdout)
  (when (not (s-blank? stdout))
    (save-excursion
      (let ((buf (get-buffer-create "*ob-ipython-stdout*")))
        (with-current-buffer buf
          (special-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert stdout)
            (goto-char (point-min))))
        (pop-to-buffer buf)))))

(defun ob-ipython--dump-error (err-msg)
  (with-current-buffer (get-buffer-create "*ob-ipython-debug*")
    (special-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert err-msg)
      (goto-char (point-min))))
  (error "There was a fatal error trying to process the request. See *ob-ipython-debug*"))

;;; process management

(defun ob-ipython--kernel-file (session-name)
  (if (s-ends-with-p ".json" name)
      name
    (format "emacs-%s.json" name)))

(defun ob-ipython--kernel-repl-cmd (name)
  (list ob-ipython-command "console" "--existing"
        (ob-ipython--kernel-file name)))

(defun ob-ipython--create-process (name cmd)
  (apply 'start-process name (format "*ob-ipython-%s*" name) (car cmd) (cdr cmd)))

(defun ob-ipython--create-kernel-driver (name &optional kernel)
  (when (not (ignore-errors (process-live-p (get-process (format "kernel-%s" name)))))
    (apply 'ob-ipython--launch-driver
           (append (list (format "kernel-%s" name))
                   (list "--conn-file" (ob-ipython--kernel-file name))
                   (if kernel (list "--kernel" kernel) '())
                   ;;should be last in the list of args
                   (if ob-ipython-kernel-extra-args
                       (list "--") '())
                   ob-ipython-kernel-extra-args))))

(defun ob-ipython--get-kernel-processes ()
  (let ((procs (-filter (lambda (p)
                          (s-starts-with? "kernel-" (process-name p)))
                        (process-list))))
    (-zip (-map (-compose (-partial 's-replace "kernel-" "")
                          'process-name)
                procs)
          procs)))

(defun ob-ipython--launch-driver (name &rest args)
  (let* ((python (locate-file (if (eq system-type 'windows-nt)
                                  "python.exe"
                                (or python-shell-interpreter "python"))
                              exec-path))
         (pargs (append (list python "--" ob-ipython-driver-path) args)))
    (ob-ipython--create-process name pargs)
    ;; give kernel time to initialize and write connection file
    (sleep-for 1)))

(defun ob-ipython--create-client-driver ()
  (when (not (ignore-errors (process-live-p (ob-ipython--get-driver-process))))
    (ob-ipython--launch-driver "client-driver" "--port"
                               (number-to-string ob-ipython-driver-port))
    ;; give driver a chance to bind to a port and start serving
    ;; requests. so horrible; so effective.
    (sleep-for 1)))

(defun ob-ipython--get-driver-process ()
  (get-process "client-driver"))

(defun ob-ipython--create-repl (name)
  ;; TODO: hack while we wait on
  ;; https://github.com/jupyter/jupyter_console/issues/93
  (let ((prev (getenv "JUPYTER_CONSOLE_TEST")))
    (setenv "JUPYTER_CONSOLE_TEST" "1")
    (run-python (s-join " " (ob-ipython--kernel-repl-cmd name)) nil nil)
    (setenv "JUPYTER_CONSOLE_TEST" prev)
    (format "*%s*" python-shell-buffer-name)))

;;; kernel management

(defun ob-ipython--choose-kernel ()
  (let ((procs (ob-ipython--get-kernel-processes)))
    (-> (ido-completing-read "kernel? " (-map 'car procs) nil t)
        (assoc procs)
        cdr
        list)))

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
    (-when-let (p (ob-ipython--get-driver-process)) (delete-process p))
    (message (format "Killed %s" (process-name proc)))))

;;; evaluation

(defun ob-ipython--execute-request (code name)
  (let ((url-request-data code)
        (url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously
                          (format "http://%s:%d/execute/%s"
                                  ob-ipython-driver-hostname
                                  ob-ipython-driver-port
                                  name))
      (if (>= (url-http-parse-response) 400)
          (ob-ipython--dump-error (buffer-string))
        (goto-char url-http-end-of-headers)
        (let ((json-array-type 'list))
          (json-read))))))

(defun ob-ipython--extract-output (msgs)
  (->> msgs
       (-filter (lambda (msg) (string= "stream" (cdr (assoc 'msg_type msg)))))
       (-filter (lambda (msg) (string= "stdout" (->> msg (assoc 'content) (assoc 'name) cdr))))
       (-map (lambda (msg) (->> msg (assoc 'content) (assoc 'text) cdr)))
       (-reduce 's-concat)))

(defun ob-ipython--extract-result (msgs)
  (->> msgs
       (-filter (lambda (msg) (-contains? '("execute_result" "display_data" "inspect_reply")
                                          (cdr (assoc 'msg_type msg)))))
       (-mapcat (lambda (msg) (->> msg
                                   (assoc 'content)
                                   (assoc 'data)
                                   cdr)))))

(defun ob-ipython--extract-error (msgs)
  (let ((error-content (->> msgs
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
       (-filter (lambda (msg) (-contains? '("execute_reply" "inspect_reply")
                                          (cdr (assoc 'msg_type msg)))))
       car
       (assoc 'content)
       (assoc 'status)
       cdr))

(defun ob-ipython--eval (service-response)
  (let ((status (ob-ipython--extract-status service-response)))
    (cond ((string= "ok" status) `((:result . ,(ob-ipython--extract-result service-response))
                                   (:output . ,(ob-ipython--extract-output service-response))))
          ((string= "abort" status) (error "Kernel execution aborted."))
          ((string= "error" status) (error (ob-ipython--extract-error service-response))))))


(defun ob-ipython--eval-async (service-response tfile)
  (let ((status (ob-ipython--extract-status service-response)))
    (cond ((string= "ok" status) `((:result . ,(ob-ipython--extract-result service-response))
                                   (:output . ,(ob-ipython--extract-output service-response))))
          ((string= "abort" status) (progn
                                      ;; (clear-async-last)
                                      (clear-async-list-on-error tfile)
                                      (error "Kernel execution aborted.")
                                      ))
          ((string= "error" status) (progn
                                      ;; (clear-async-last)
                                      (clear-async-list-on-error tfile)
                                      (error (ob-ipython--extract-error service-response))
                                      )))))


;;; inspection

(defun ob-ipython--inspect-request (code &optional pos detail)
  (let ((url-request-data (json-encode `((code . ,code)
                                         (pos . ,(or pos (length code)))
                                         (detail . ,(or detail 0)))))
        (url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously
                          ;; TODO: hardcoded the default session here
                          (format "http://%s:%d/inspect/default"
                                  ob-ipython-driver-hostname
                                  ob-ipython-driver-port))
      (if (>= (url-http-parse-response) 400)
          (ob-ipython--dump-error (buffer-string))
        (goto-char url-http-end-of-headers)
        (let ((json-array-type 'list))
          (json-read))))))

(defun ob-ipython--inspect (buffer pos)
  (let* ((code (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
         (resp (ob-ipython--inspect-request code pos 0))
         (status (ob-ipython--extract-status resp)))
    (if (string= "ok" status)
        (ob-ipython--extract-result resp)
      (error (ob-ipython--extract-error resp)))))

(defun ob-ipython-inspect (buffer pos)
  "Ask a kernel for documentation on the thing at POS in BUFFER."
  (interactive (list (current-buffer) (point)))
  (-if-let (result (->> (ob-ipython--inspect buffer pos) (assoc 'text/plain) cdr))
      (ob-ipython--create-inspect-buffer result)
    (message "No documentation was found.")))

;;; babel framework

(add-to-list 'org-src-lang-modes '("ipython" . python))

(defvar org-babel-default-header-args:ipython '())

(defun ob-ipython--normalize-session (session)
  (if (string= "default" session)
      (error "default is reserved for when no name is provided. Please use a different session name.")
    (or session "default")))


;; Code for both sync and async
(setq async-list ())
(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."

  (if (cdr (assoc :async params))
      (let* ((current-file (buffer-file-name))
             (code (org-element-property :value (org-element-context)))
             (temporary-file-directory ".")
             (tempfile (make-temp-file "py-"))
             (file (cdr (assoc :async-file params))) ;; use async-file instead of file
             (session (cdr (assoc :session params)))
             (result-type (cdr (assoc :result-type params)))
             )
        (org-babel-remove-result)
        (setq async-list (cons tempfile (cons (buffer-file-name) (cons result-type (cons file async-list)))))
        ;; (org-babel-ipython-initiate-session session)
        (ob-ipython-async--execute-request
         (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                        params (org-babel-variable-assignments:python params))
         (ob-ipython--normalize-session session)
         tempfile
         )
        tempfile)
    (let* ((file (cdr (assoc :file params)))
           (session (cdr (assoc :session params)))
           (result-type (cdr (assoc :result-type params))))
      (org-babel-ipython-initiate-session session params)
      (-when-let (ret (ob-ipython--eval
                       (ob-ipython--execute-request
                        (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                       params (org-babel-variable-assignments:python params))
                        (ob-ipython--normalize-session session))))
        (let ((result (cdr (assoc :result ret)))
              (output (cdr (assoc :output ret))))
          (if (eq result-type 'output)
              output
            (ob-ipython--create-stdout-buffer output)
            (cond ((and file (string= (f-ext file) "png"))
                   (->> result (assoc 'image/png) cdr (ob-ipython--write-base64-string file)))
                ((and file (string= (f-ext file) "svg"))
                 (->> result (assoc 'image/svg+xml) cdr (ob-ipython--write-string-to-file file)))
                (file (error "%s is currently an unsupported file extension." (f-ext file)))
                (t (->> result reverse (assoc 'text/plain) cdr ob-ipython--table-or-string))))))))

(defun ob-ipython--table-or-string (results)
  "Extract an Org table from RESULTS if it looks like it might be
a table."
  (when results
    (org-babel-python-table-or-string results)))

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
    (ob-ipython--create-client-driver)
    (when (not (s-ends-with-p ".json" session))
      (ob-ipython--create-kernel-driver (ob-ipython--normalize-session session)
                                        (cdr (assoc :kernel params))))
    (ob-ipython--create-repl (ob-ipython--normalize-session session))))


;; Additional function for async

(defun ob-ipython-async--execute-request (code name tempfile)
  (let ((url-request-data code)
        (url-request-method "POST")
        (json-array-type 'list)
        )

    (with-temp-buffer tempfile
                      (url-retrieve
                       (format "http://%s:%d/execute/%s"
                               ob-ipython-driver-hostname
                               ob-ipython-driver-port
                               name)
                       (lambda (outp tfile)
                         (if (>= (url-http-parse-response) 400)
                             (progn
                               (clear-async-list-on-error tfile)
                               (ob-ipython--dump-error outp))
                           (goto-char url-http-end-of-headers)
                           (flush-lines "^\s*$" nil nil t)
                           (kill-region (point) (point-min))
                           (write-region (buffer-string) nil tfile)
                           (ipython--async-sentinel tfile)
                           )
                         )
                       (cons tempfile ())
                       )
                      ))
  )

(defun ipython--async-sentinel (tempfile)
  ;; Make sentinel for post url-retrive

  (let* ((current-file (buffer-file-name))
         (jr (load_json_from_file tempfile))
         (ret (ob-ipython--eval-async jr tempfile))
         (result (cdr (assoc :result ret)))
         (output (cdr (assoc :output ret)))
         (fi (-elem-index tempfile async-list))
         (tmp (-split-at fi async-list))
         (ltail (car (cdr tmp)))
         (org-filename (nth 1 ltail))
         (result-type (nth 2 ltail))
         (file (nth 3 ltail))
         )

    (save-window-excursion
      (save-excursion
        (save-restriction
          (with-current-buffer (find-file-noselect org-filename)
            (goto-char (point-min))
            (re-search-forward tempfile)
            (beginning-of-line)
            (kill-line)
            (if (> (length file) 0)
                (insert (concat "[[file:" file "]]"))
              (insert ": ")
              )
             (if (eq result-type 'output)
                 output
               (ob-ipython--create-stdout-buffer output)
               (cond ((and file (string= (f-ext file) "png"))
                      (->> result (assoc 'image/png) cdr (ob-ipython--write-base64-string file))
                      (org-redisplay-inline-images)
                      )
                     ((and file (string= (f-ext file) "svg"))
                      (->> result (assoc 'image/svg+xml) cdr (ob-ipython--write-string-to-file file))
                      (org-redisplay-inline-images)
                      )
                     (file (error "%s is currently an unsupported file extension." (f-ext file)))
                     (t (insert (->> result (assoc 'text/plain) cdr)))
                     )
               )
            ))))
    ;; Clear variable and delete temp file
    ;; (setq async-list (cons (car tmp) (-drop 4 ltail)))
    (setq async-list (-remove-at-indices (-map (lambda (n) (+ fi n)) '(0 1 2 3)) async-list))
    (delete-file tempfile)
    )
  )

(defun clear-async-list-on-error(tempfile)
  (let* ((fi (-elem-index tempfile async-list))
         (tmp (-split-at fi async-list))
         (ltail (car (cdr tmp)))
         (org-filename (nth 1 ltail)))

    ;; (setq async-list (cons (car tmp) (-drop 4 ltail)))
    (setq async-list (-remove-at-indices (-map (lambda (n) (+ fi n)) '(0 1 2 3)) async-list))
    (delete-file tempfile)

    (save-window-excursion
      (save-excursion
        (save-restriction
          (with-current-buffer (find-file-noselect org-filename)
            (goto-char (point-min))
            (re-search-forward tempfile)
            (beginning-of-line)
            (kill-line)
            (insert ": Error !!")
            ))))
    )
  )

(defun clear-async-last()
  (let* ((tempfile (car async-list))
         (fi (-elem-index tempfile async-list))
         (tmp (-split-at fi async-list))
         (ltail (car (cdr tmp))))

    (setq async-list (cons (car tmp) (-drop 4 ltail)))

    (delete-file tempfile)
    )
  )

(defun load_json_from_file(file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((json-array-type 'list))
      (json-read))
    )
  )

(defun clear-async-all()
  (let* ((tempfile (car async-list))
         (fi (-elem-index tempfile async-list))
         (tmp (-split-at fi async-list))
         (ltail (car (cdr tmp))))

    (setq async-list (cons (car tmp) (-drop 4 ltail)))
    (setq async-list (-remove-at-indices (-map (lambda (n) (+ fi n)) '(0 1 2 3)) async-list))
    (delete-file tempfile)
    )
)

(provide 'ob-ipython)

;;; ob-ipython.el ends here
