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

;;; babel framework

(add-to-list 'org-src-lang-modes '("ipython" . python))

(defvar org-babel-default-header-args:ipython '())

(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."
  (debug-msg body)
  (debug-msg params)
  ;; TODO:
  7)

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

;;; evaluation

(provide 'ob-python)

;;; ob-python.el ends here
