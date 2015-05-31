;;; ob-ipython.el --- org-babel functions for IPython evaluation

;; Author: Greg Sexton <gregsexton@gmail.com>
;; Keywords: literate programming, reproducible research
;; Homepage: http://www.gregsexton.org

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
(require 'dash)
(require 's)
(require 'f)
(require 'json)

;;; variables

(defcustom ob-ipython-kernel-extra-args '()
  "List of extra args to pass when creating a kernel."
  :group 'ob-ipython)

(defcustom ob-ipython-driver-port 9988
  "Port to use for http driver."
  :group 'ob-ipython)

(defcustom ob-ipython-driver-path
  (f-expand "./driver.py"
            (or (-when-let (f load-file-name) (f-dirname f)) default-directory))
  "Path to the driver script."
  :group 'ob-ipython)

;;; utils

(defun ob-ipython--write-base64-string (file b64-string)
  (with-temp-buffer
  (insert b64-string)
  (base64-decode-region (point-min) (point-max))
  (write-file file)))

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

;;; process management

(defun ob-ipython--kernel-cmd (name)
  (-concat (list "ipython" "kernel" (format "--IPKernelApp.connection_file=emacs-%s.json" name))
           ob-ipython-kernel-extra-args))

(defun ob-ipython--kernel-repl-cmd (name)
  (list "ipython" "console" "--existing" (format "emacs-%s.json" name)))

(defun ob-ipython--create-process (name cmd)
  (apply 'start-process name (format "*ob-ipython-%s*" name) (car cmd) (cdr cmd)))

(defun ob-ipython--create-kernel (name)
  (when (not (process-live-p (get-process (format "kernel-%s" name))))
    (ob-ipython--create-process (format "kernel-%s" name) (ob-ipython--kernel-cmd name))))

(defun ob-ipython--create-driver ()
  (when (not (process-live-p (get-process "ob-ipython-driver")))
    (ob-ipython--create-process "ob-ipython-driver"
                                (list (locate-file "python" exec-path)
                                      ob-ipython-driver-path
                                      (number-to-string ob-ipython-driver-port)))
    ;; give driver a chance to bind to a port and start serving
    ;; requests. so horrible; so effective.
    (sleep-for 1)))

(defun ob-ipython--create-repl (name)
  (let ((python-shell-buffer-name (format "ob-ipy-repl-%s" name)))
    (run-python (s-join " " (ob-ipython--kernel-repl-cmd name))
                nil nil)
    (format "*%s*" python-shell-buffer-name)))

;;; evaluation

(defun ob-ipython--execute-request (code name)
  (let ((url-request-data code)
        (url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously
                          (format "http://localhost:%d/execute/%s" ob-ipython-driver-port name))
      (if (>= (url-http-parse-response) 400)
          ;; TODO: output to a debug buffer
          (error "Got an error back from the service. See *ob-ipython-debug*")
        (goto-char url-http-end-of-headers)
        (let ((json-array-type 'list))
          (json-read))))))

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
    (cond ((string= "ok" status) (ob-ipython--extract-result service-response))
          ((string= "abort" status) (error "Kernel execution aborted."))
          ((string= "error" status) (error (ob-ipython--extract-error service-response))))))

;;; inspection

(defun ob-ipython--inspect-request (code &optional pos detail)
  (let ((url-request-data (json-encode `((code . ,code)
                                         (pos . ,(or pos (length code)))
                                         (detail . ,(or detail 0)))))
        (url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously
                          ;; TODO: hardcoded the nil session here
                          (format "http://localhost:%d/inspect/nil" ob-ipython-driver-port))
      (if (>= (url-http-parse-response) 400)
          ;; TODO: output to a debug buffer
          (error "Got an error back from the service. See *ob-ipython-debug*")
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
      (error (ob-ipython--extract-error service-response)))))

(defun ob-ipython-inspect (buffer pos)
  (interactive (list (current-buffer) (point)))
  (-if-let (result (->> (ob-ipython--inspect buffer pos) (assoc 'text/plain) cdr))
      (let ((buf (get-buffer-create "*ob-ipython-inspect*")))
        (with-current-buffer buf
          (special-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert result)
            (ansi-color-apply-on-region (point-min) (point-max))
            (whitespace-cleanup)
            (goto-char (point-min))))
        (pop-to-buffer buf))
    (message "No documentation was found.")))

;;; babel framework

(add-to-list 'org-src-lang-modes '("ipython" . python))

(defvar org-babel-default-header-args:ipython '())

;;; TODO: probably need some kind of behaviour lookup based on passed
;;; in params and what I'm holding.

;;; TODO: need to check file extension of file

(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."
  (debug-msg params)
  (let* ((file (cdr (assoc :file params)))
         (session (cdr (assoc :session params))))
    (org-babel-ipython-initiate-session session)
    (-when-let (result (ob-ipython--eval (ob-ipython--execute-request body session)))
      (if file
          (->> result (assoc 'image/png) cdr (ob-ipython--write-base64-string file))
        (->> result (assoc 'text/plain) cdr)))))

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
    (ob-ipython--create-driver)
    (ob-ipython--create-kernel session)
    (ob-ipython--create-repl session)))

(provide 'ob-ipython)

;;; ob-ipython.el ends here
