(setq smerge-packages '(smerge-mode))

(defun smerge/init-smerge-mode ()
  (use-package smerge-mode
    :demand t
    :config
    (progn
      (evil-make-overriding-map smerge-mode-map 'normal t)
      (spacemacs|define-micro-state smerge-action
        :doc "[,] prev [.] next  [r]efine  [k]ill current
keep [a]ll [m]ine [o]ther [b]ase [c]urrent
[n] combine [N] auto-combine"
        :execute-binding-on-enter t
        :bindings
        ("," smerge-prev)
        ("." smerge-next)
        ("a" smerge-keep-all)
        ("m" smerge-keep-mine)
        ("o" smerge-keep-other)
        ("b" smerge-keep-base)
        ("c" smerge-keep-current)
        ("k" smerge-kill-current)
        ("r" smerge-refine)
        ("n" smerge-combine-with-next)
        ("N" smerge-auto-combine))
      (evil-define-key 'normal smerge-mode-map
        "Sdbm" 'smerge-diff-base-mine
        "Sdmo" 'smerge-diff-mine-other
        "Sdbo" 'smerge-diff-base-other
        "Sa" 'spacemacs/smerge-action-micro-state
        "Sm" 'spacemacs/smerge-action-micro-state
        "So" 'spacemacs/smerge-action-micro-state
        "Sb" 'spacemacs/smerge-action-micro-state
        "Sc" 'spacemacs/smerge-action-micro-state
        "Sk" 'spacemacs/smerge-action-micro-state
        "Sr" 'spacemacs/smerge-action-micro-state
        "S," 'spacemacs/smerge-action-micro-state
        "S." 'spacemacs/smerge-action-micro-state
        "Sn" 'spacemacs/smerge-action-micro-state
        "SN" 'spacemacs/smerge-action-micro-state)
      (defun guide-key/smerge-mode-hook ()
        (guide-key/add-local-guide-key-sequence "S")
        (guide-key/add-local-guide-key-sequence "Sd"))
      (add-hook 'find-file-hook #'smerge-start-session)
      (add-hook 'smerge-mode-hook #'guide-key/smerge-mode-hook)
      (add-hook 'smerge-mode-hook #'evil-normalize-keymaps))))
