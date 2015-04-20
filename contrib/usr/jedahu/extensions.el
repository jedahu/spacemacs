(defvar jedahu-post-extensions
  '(vbnet-mode))

(defun jedahu/init-vbnet-mode ()
  (use-package vbnet-mode
    :defer t
    :mode "\\.vb\\'"
    ))
