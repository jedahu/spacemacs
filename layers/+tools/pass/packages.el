(setq pass-packages '(pass))

(defun pass/init-pass ()
  (use-package pass
    :defer t
    :commands (pass)
    :config
    (progn
      (evilified-state-evilify-map pass-mode-map
        :mode pass-
        :bindings
        (kbd "RET") 'pass-view
        ))))
