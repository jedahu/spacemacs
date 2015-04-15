(defun end-of-chunk ()
  "forward line or to ends of mid-expression."
  (interactive)
  (goto-char (point-at-eol))
  (let ((limit (point-at-bol))
        temp
        expr-beg)
    (while (and (setq temp (nth 1 (syntax-ppss)))
                (<= limit temp))
      (goto-char temp)
      (setq expr-beg (point)))
    (when expr-beg
        (goto-char expr-beg)
      (forward-sexp))))

(defun sort-lines-as-exprs (reverse beg end)
  "sort lines, or whole expression if line ends mid-expression."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse
                 'forward-line
                 'end-of-chunk))))
