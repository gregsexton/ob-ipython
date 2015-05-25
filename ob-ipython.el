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

;;; babel framework

(add-to-list 'org-src-lang-modes '("ipython" . python))

(defvar org-babel-default-header-args:ipython '())

(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."
  (ob-ipython--create-kernel "default")
  (ob-ipython--create-driver)
  (ob-ipython--eval (ob-ipython--execute-request body)))

(defun org-babel-prep-session:ipython (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references"
  ;; TODO: c-u c-c c-v c-z
  (debug-msg "we're prepping!"))

(defun org-babel-load-session:ipython (session body params)
  "Load BODY into SESSION."
  ;; TODO: c-c c-v c-l
  (debug-msg "we're loading!"))

;;; TODO: create a session idempotently and then connect a repl using --existing
;;; TODO: do I need to do my own idempotency?
(defun org-babel-ipython-initiate-session (&optional session params)
  "Create a session named SESSION according to PARAMS."
  ;; TODO: c-c c-v c-z
  (unless (string= session "none")
    (debug-msg "initiate sessh!")))

;;; process management

;;; TODO: figure out names and who owns them
(defun ob-ipython--kernel-cmd (name)
  (-concat (list "ipython" "kernel" (format "--IPKernelApp.connection_file=emacs-%s.json" name))
           ob-ipython-kernel-extra-args))

(defun ob-ipython--create-process (name cmd)
  (apply 'start-process name (format "*ob-ipython-%s*" name) (car cmd) (cdr cmd)))

(defun ob-ipython--create-kernel (name)
  (when (not (process-live-p (get-process (format "kernel-%s" name))))
    (ob-ipython--create-process (format "kernel-%s" name) (ob-ipython--kernel-cmd name))))

;;; TODO: think there's a race here. the process takes a moment to get
;;; warm but we issue a request immediately
(defun ob-ipython--create-driver ()
  (when (not (process-live-p (get-process "ob-ipython-driver")))
    (ob-ipython--create-process "ob-ipython-driver"
                                (list (locate-file "python" exec-path)
                                      ob-ipython-driver-path
                                      (number-to-string ob-ipython-driver-port)))))

;;; evaluation

(defun ob-ipython--execute-request (code)
  (let ((url-request-data code)
        (url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously
                          (format "http://localhost:%d/execute" ob-ipython-driver-port))
      (if (>= (url-http-parse-response) 400)
          ;; TODO: output to a debug buffer
          (error "Got an error back from the service. See *ob-ipython-debug*")
        (goto-char url-http-end-of-headers)
        (let ((json-array-type 'list))
          (json-read))))))

(defun ob-ipython--eval (service-response)
  (->> service-response
       (-filter (lambda (msg) (string= (cdr (assoc 'msg_type msg)) "execute_result")))
       car
       (assoc 'content)
       (assoc 'data)
       (assoc 'text/plain)
       cdr))

(provide 'ob-ipython)

;;; ob-ipython.el ends here
