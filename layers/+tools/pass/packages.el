(setq pass-packages '(pass))

(defun pass/init-pass ()
  (use-package pass
    :defer t
    :commands (pass)
    :config
    (progn
      (evilified-state-evilify pass-mode pass-mode-map
        (kbd "RET") 'pass-view
        "y" 'pass-copy
        "j" 'pass-next-entry
        "k" 'pass-prev-entry
        "gj" 'pass-next-directory
        "gk" 'pass-prev-directory
        "d" 'pass-kill
        "i" 'pass-insert
        "c" 'pass-rename
        "q" 'pass-quit
        "r" 'pass-update-buffer
        ))))
