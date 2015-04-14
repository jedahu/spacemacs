(defvar revealjs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    map)
  "Keymap for revealjs-mode.")

(define-derived-mode revealjs-mode markdown-mode "RevealJs"
  "Major mode for editing RevealJs markdown files.")

(defun revealjs--scroll (direction regex)
  (forward-line direction)
  (funcall (if (> direction 0) 're-search-forward 're-search-backward)
           regex nil 1)
  (forward-line 0)
  (recenter 0))

(defvar revealjs-horizontal-separator "^---+$"
  "Regex separating horizontal slides.")

(defvar revealjs-vertical-separator "^- -\\( -\\)+ ?$"
  "Regex separating vertical slides.")

(defvar revealjs-notes-separator "^\\* \\* \\*$"
  "Regex separating slide notes from slide contents.")

(defvar revealjs-horizontal-string (make-string 80 ?-)
  "String to insert between horizontal slides.")

(defvar revealjs-vertical-string
  (string-trim-right (apply 'concat (make-list 40 "- ")))
  "String to insert between vertical slides.")

(defvar revealjs-notes-string "* * *"
  "String to insert between slide content and notes.")

(defun revealjs--slide-separator ()
  (concat
   "\\("
   revealjs-horizontal-separator
   "\\)\\|\\("
   revealjs-vertical-separator
   "\\)"))

(defun revealjs-next-slide ()
  (interactive)
  (revealjs--scroll 1 (revealjs--slide-separator)))

(defun revealjs-prev-slide ()
  (interactive)
  (revealjs--scroll -1 (revealjs--slide-separator)))

(defun revealjs-next-horizontal-slide ()
  (interactive)
  (revealjs--scroll 1 revealjs-horizontal-separator))

(defun revealjs-prev-horizontal-slide ()
  (interactive)
  (revealjs--scroll -1 revealjs-horizontal-separator))

(defun revealjs--insert-slide (separator)
  (forward-line 1)
  (insert "\n" separator "\n\n"))

(defun revealjs-insert-horizontal-slide ()
  (interactive)
  (revealjs--insert-slide revealjs-horizontal-string))

(defun revealjs-insert-vertical-slide ()
  (interactive)
  (revealjs--insert-slide revealjs-vertical-string))

(provide 'revealjs-mode)
