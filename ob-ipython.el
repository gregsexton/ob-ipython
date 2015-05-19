;;; ob-ipython.el --- org-babel functions for IPython evaluation

;; Copyright (C) 2009-2014 Free Software Foundation, Inc.

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
(eval-when-compile (require 'cl))

(defun org-babel-execute:ipython (body params)
  "Execute a block of iPython code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-python-initiate-session
                   (cdr (assoc :session params))))
         (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
         (return-val (when (and (eq result-type 'value) (not session))
                       (cdr (assoc :return params))))
         (preamble (cdr (assoc :preamble params)))
         (full-body
          (org-babel-expand-body:generic
           (concat body (if return-val (format "\nreturn %s" return-val) ""))
           params (org-babel-variable-assignments:python params)))
         (result (org-babel-python-evaluate
                  session full-body result-type result-params preamble)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assoc :colname-names params))
                          (cdr (assoc :colnames params)))
     (org-babel-pick-name (cdr (assoc :rowname-names params))
                          (cdr (assoc :rownames params))))))

(provide 'ob-python)

;;; ob-python.el ends here
