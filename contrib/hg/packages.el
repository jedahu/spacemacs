(defvar hg-packages
  '(
    monky
    ))

(defun hg/init-monky ()
  (use-package monky
    :defer t
    :commands (monky-status monky-log monky-blame-mode monky-queue
               monky-hg-command)
    :init
    (progn
      (evil-leader/set-key
        "Mb" 'monky-blame-mode
        "Ml" 'monky-log
        "Ms" 'monky-status
        "Mq" 'monky-queue
        "M:" 'monky-hg-command
        ;; "MC" 'magit-commit
        )
      (evilify monky-commit-mode monky-commit-mode-map
               (kbd "C-j") 'monky-goto-next-section
               (kbd "C-k") 'monky-goto-previous-section
               (kbd "C-n") 'monky-goto-next-section
               (kbd "C-p") 'monky-goto-previous-section
               (kbd "C-v") 'monky-revert-file)
      (evilify monky-log-mode monky-log-mode-map
               (kbd "C-j") 'monky-goto-next-section
               (kbd "C-k") 'monky-goto-previous-section
               (kbd "C-n") 'monky-goto-next-section
               (kbd "C-p") 'monky-goto-previous-section
               (kbd "C-v") 'monky-revert-file)
      ;; (evilify magit-process-mode magit-process-mode-map
      ;;          (kbd "C-j") 'magit-goto-next-section
      ;;          (kbd "C-k") 'magit-goto-previous-section
      ;;          (kbd "C-n") 'magit-goto-next-section
      ;;          (kbd "C-p") 'magit-goto-previous-section
      ;;          (kbd "C-v") 'magit-revert-item)
      (evilify monky-branches-mode monky-branches-mode-map
               "K" 'monky-discard-item
               ;; "L" 'magit-key-mode-popup-logging
               (kbd "C-j") 'monky-goto-next-section
               (kbd "C-k") 'monky-goto-previous-section
               (kbd "C-n") 'monky-goto-next-section
               (kbd "C-p") 'monky-goto-previous-section
               (kbd "C-v") 'monky-revert-file)
      (evilify monky-status-mode monky-status-mode-map
               "K" 'monky-discard-item
               ;; "L" 'magit-key-mode-popup-logging
               ;; "H" 'magit-key-mode-popup-diff-options
               (kbd "C-j") 'monky-goto-next-section
               (kbd "C-k") 'monky-goto-previous-section
               (kbd "C-n") 'monky-goto-next-section
               (kbd "C-p") 'monky-goto-previous-section
               (kbd "C-v") 'monky-revert-file)
      (evilify monky-queue-mode monky-queue-mode-map
               (kbd "C-j") 'monky-goto-next-section
               (kbd "C-k") 'monky-goto-previous-section
               (kbd "C-n") 'monky-goto-next-section
               (kbd "C-p") 'monky-goto-previous-section
               (kbd "C-v") 'monky-revert-file))
    :config
    (progn
      ;; (spacemacs|hide-lighter magit-auto-revert-mode)
      ;; full screen magit-status
      ;; (when git-magit-status-fullscreen
      ;;   (defadvice magit-status (around magit-fullscreen activate)
      ;;     (window-configuration-to-register :magit-fullscreen)
      ;;     ad-do-it
      ;;     (delete-other-windows))

      ;;   (defun magit-quit-session ()
      ;;     "Restores the previous window configuration and kills the magit buffer"
      ;;     (interactive)
      ;;     (kill-buffer)
      ;;     (jump-to-register :magit-fullscreen))
      ;;   (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

      ;; (defun magit-toggle-whitespace ()
      ;;   (interactive)
      ;;   (if (member "-w" magit-diff-options)
      ;;       (magit-dont-ignore-whitespace)
      ;;     (magit-ignore-whitespace)))

      ;; (defun magit-ignore-whitespace ()
      ;;   (interactive)
      ;;   (add-to-list 'magit-diff-options "-w")
      ;;   (magit-refresh))

      ;; (defun magit-dont-ignore-whitespace ()
      ;;   (interactive)
      ;;   (setq magit-diff-options (remove "-w" magit-diff-options))
      ;;   (magit-refresh))
      ;; (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
      )))
