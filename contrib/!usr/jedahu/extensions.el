(setq jedahu-post-extensions
  '(
    liquid-mode
    vbnet-mode))

(defun jedahu/init-liquid-mode ()
  (use-package liquid-mode
    :defer t
    :mode "\\.liquid\\'"))

(defun jedahu/init-vbnet-mode ()
  (use-package vbnet-mode
    :defer t
    :mode "\\.vb\\'"
    ))
