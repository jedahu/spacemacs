(defvar smerge-packages '(smerge))

(defun smerge/init-smerge ()
  (use-package smerge
    :defer t
    :config
    (progn
      (setq spacemacs/key-binding-prefixes '(("mk" . "Keep")))
      (evil-leader/set-key-for-mode 'smerge-mode
        "mka" 'smerge-keep-all
        "mkm" 'smerge-keep-mine
        "mko" 'smerge-keep-other
        "mkb" 'smerge-keep-base
        "mkc" 'smerge-keep-current
        "mKc" 'smerge-kill-current
        "cr" 'smerge-refine
        "Dbm" 'smerge-diff-base-mine
        "Dmo" 'smerge-diff-mine-other
        "Dbo" 'smerge-diff-base-other))))
