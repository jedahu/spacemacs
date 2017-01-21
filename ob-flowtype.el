;;; ob-flowtype.el --- org-babel functions for flowtype evaluation

;; Copyright (C) 2015 KURASHIKI Satoru

;; Author: KURASHIKI Satoru
;; Keywords: literate programming, reproducible research, flowtype
;; Homepage: https://github.com/lurdan/ob-flowtype
;; Version: 0.1
;; Package-Requires: ((emacs "24") (org "8.0"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Exec flowtype in org-babel code blocks.

;;; Requirements:
;; You need to install node.js and flowtype to use this extension.

;;; Code:
(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("flowtype" . "js"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:flowtype
  '((:cmdline . "check-contents --color=always")
    (:wrap . "ANSI")))

(defun org-babel-flowtype-var-to-flowtype (var)
  "Convert an elisp var into a string of flowtype source code
specifying a var of the same value."
  (format "%S" var))

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:flowtype' function below.
(defun org-babel-expand-body:flowtype (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  body)

(defun org-babel-execute:flowtype (body params)
  "Execute a block of Flowtype code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (let* ((tmp-flowconfig (concat org-babel-temporary-directory "/.flowconfig"))
         (tmp-src (org-babel-temp-file "flow-src-" ".js"))
         (cmdline (cdr (assoc :cmdline params)))
         (cmdline (if cmdline (concat " " cmdline) ""))
         (run (string= "yes" (cdr (assoc :run params)))))
    (unless (file-exists-p tmp-flowconfig)
      (f-touch (concat org-babel-temporary-directory "/.flowconfig")))
    (let ((default-directory org-babel-temporary-directory))
      (with-temp-file tmp-src (insert body))
      (with-temp-buffer
        (call-process-shell-command
         (if run "babel -f _.js | node" (format "flow %s" cmdline))
         tmp-src (current-buffer))
        (buffer-string)))))

(define-derived-mode flowtype-mode js-mode "Flow" "Major mode for flow")

(provide 'ob-flowtype)

;;; ob-flowtype.el ends here
