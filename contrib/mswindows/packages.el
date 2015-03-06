(defvar mswindows-packages '(projectile))

(defun mswindows/init-projectile ()
  (use-package projectile
    :config
    (progn
      (setq projectile-indexing-method 'alien))))
