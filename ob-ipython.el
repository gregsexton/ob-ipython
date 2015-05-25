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

;;; babel framework

(add-to-list 'org-src-lang-modes '("ipython" . python))

(defvar org-babel-default-header-args:ipython '())

(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."
  (debug-msg body)
  (debug-msg params)
  (ob-ipython--eval (ob-ipython--execute-request body)))

(defun org-babel-prep-session:ipython (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references"
  ;; TODO: c-u c-c c-v c-z
  (debug-msg "we're prepping!")
  )

(defun org-babel-load-session:ipython (session body params)
  "Load BODY into SESSION."
  ;; TODO: c-c c-v c-l
  (debug-msg "we're loading!")
  )

(defun org-babel-ipython-initiate-session (&optional session params)
  "Create a session named SESSION according to PARAMS."
  ;; TODO: c-c c-v c-z
  (unless (string= session "none")
    (debug-msg "initiate sessh!")))

;;; process management

;; (progn
;;   ;; (kill-process "ipython-driver")
;;   (debug-clear)
;;   (let ((process (start-process "ipython-driver" nil "python" "./driver.py")))
;;     (set-process-filter process (lambda (proc output)
;;                                   (debug-msg "-----")
;;                                   (debug-msg output)
;;                                   (debug-msg "-----")
;;                                   (debug-msg "")
;;                                   (debug-msg (json-read-from-string output))))
;;     (set-process-sentinel process (lambda (proc event)
;;                                     (debug-msg proc)
;;                                     (debug-msg event)
;;                                     (when (not (process-live-p proc))
;;                                       (if (not (= 0 (process-exit-status process)))
;;                                           (amz/error "Process %s exited with status code %d"
;;                                                      executable (process-exit-status process))
;;                                         (funcall callback)))))))

;;; evaluation

(defun ob-ipython--execute-request (code)
  (let ((url-request-data code)
        (url-request-method "POST"))
    ;; TODO: port
    (with-current-buffer (url-retrieve-synchronously "http://localhost:8888/execute")
      (when (equal (url-http-parse-response) 200)
        (goto-char url-http-end-of-headers)
        (let ((json-array-type 'list))
          (json-read))))))

(defun ob-ipython--eval (service-response)
  (-if-let (resp service-response)
      (->> resp
           (-filter (lambda (msg) (string= (cdr (assoc 'msg_type msg)) "execute_result")))
           car
           (assoc 'content)
           (assoc 'data)
           (assoc 'text/plain)
           cdr)
    ;; TODO: output to a debug buffer
    (error "Got an error back from the service. See *ob-ipython-debug*")))

(provide 'ob-python)

;;; ob-python.el ends here
