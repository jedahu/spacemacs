(setq jedahu-packages
  '(
    ahg
    cc-mode
    ediff
    eshell
    eshell-autojump
    git
    helm
    parent-mode
    rcirc
    spinner
    ))

(defun jedahu-setup-pcomplete-hg ()
  (defun pcmpl-hg-commands ()
    "Return the most common hg commands by parsing the hg output."
    (with-temp-buffer
      (call-process-shell-command "hg" nil (current-buffer) nil "-v" "help")
      (goto-char 0)
      (search-forward "list of commands:")
      (let (commands
            (bound (save-excursion
                     (re-search-forward "^[[:alpha:]]")
                     (forward-line 0)
                     (point))))
        (while (re-search-forward
                "^[[:blank:]]\\([[:word:]]+\\(?:, [[:word:]]+\\)*\\)" bound t)
          (let ((match (match-string 1)))
            (if (not (string-match "," match))
                (push (match-string 1) commands)
              (dolist (c (split-string match ", ?"))
                (push c commands)))))
        (sort commands #'string<))))

  (defconst pcmpl-hg-commands (pcmpl-hg-commands)
    "List of `hg' commands.")

  (defun pcomplete/hg ()
    "Completion for `hg'."
    ;; Completion for the command argument.
    (pcomplete-here* pcmpl-hg-commands)
    (cond
     ((pcomplete-match "help" 1)
      (pcomplete-here* pcmpl-hg-commands))
     (t
      (while (pcomplete-here (pcomplete-entries)))))))

(defun jedahu-setup-pcomplete-git ()
  (defun pcmpl-git-commands ()
    "Return the most common git commands by parsing the git output."
    (with-temp-buffer
      (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
      (goto-char 0)
      (search-forward "available git commands in")
      (let (commands)
        (while (re-search-forward
                "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)[[:blank:]]+\\([[:word:]-.]+\\)?"
                nil t)
          (push (match-string 1) commands)
          (push (match-string 2) commands)
          (when (match-string 3)
            (push (match-string 3) commands)))
        (sort commands #'string<))))

  (defconst pcmpl-git-commands (pcmpl-git-commands)
    "List of `git' commands.")

  (defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
    "The `git' command to run to get a list of refs.")

  (defun pcmpl-git-get-refs (type)
    "Return a list of `git' refs filtered by TYPE."
    (with-temp-buffer
      (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
      (goto-char (point-min))
      (let (refs)
        (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
          (push (match-string 1) refs))
        (nreverse refs))))

  (defun pcmpl-git-remotes ()
    "Return a list of remote repositories."
    (split-string (shell-command-to-string "git remote")))

  (defun pcomplete/git ()
    "Completion for `git'."
    ;; Completion for the command argument.
    (pcomplete-here* pcmpl-git-commands)
    (cond
     ((pcomplete-match "help" 1)
      (pcomplete-here* pcmpl-git-commands))
     ((pcomplete-match (regexp-opt '("pull" "push")) 1)
      (pcomplete-here (pcmpl-git-remotes)))
     ;; provide branch completion for the command `checkout'.
     ((pcomplete-match "checkout" 1)
      (pcomplete-here* (append (pcmpl-git-get-refs "heads")
                               (pcmpl-git-get-refs "tags"))))
     (t
      (while (pcomplete-here (pcomplete-entries)))))))

(defun jedahu/init-rcirc ()
  (eval-after-load 'rcirc
    '(progn
       (message "jedahu: rcirc.")
       (setq rcirc-server-alist
             '(("irc.freenode.net" :port 6697 :encryption tls
                :channels ("#nixos" "#purescript"))))
       (setq rcirc-authinfo-file
             (concat user-emacs-directory "rcirc-authinfo")))))

(defun jedahu/init-spinner ()
  (message "jedahu: spinner"))

(defun jedahu/init-ediff ()
  (eval-after-load 'ediff
    '(progn
       (defun ediff-copy-both-to-C ()
         (interactive)
         (ediff-copy-diff ediff-current-difference nil 'C nil
                          (concat
                           (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                           (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

       (defun add-d-to-ediff-mode-map ()
         (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))

       (add-hook 'ediff-keymap-setup-hook'add-d-to-ediff-mode-map))))

(defun jedahu-setup-eshell ()
  (defclass helm-eshell-cd-history-source (helm-source-in-buffer)
    ((init :initform (lambda ()
                       (let ((eshell-last-dir-unique t))
                         (eshell-write-last-dir-ring)
                         (with-current-buffer (helm-candidate-buffer 'global)
                           (insert-file-contents eshell-last-dir-ring-file-name)))
                       (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
     (nomark :initform t)
     (keymap :initform helm-eshell-history-map)
     (filtered-candidate-transformer
      :initform (lambda (candidates sources)
                  (reverse candidates)))
     (candidate-number-limit :initform 9999)
     (action :initform (lambda (candidate)
                         (eshell-kill-input)
                         (cd candidate)
                         (eshell-reset))))
    "Helm class to define source for Eshell directory history.")

  (defun helm-eshell-cd-history ()
    "Preconfigured helm for eshell directory history."
    (interactive)
    (helm :sources (helm-make-source "Eshell directory history"
                       'helm-eshell-cd-history-source)
          :buffer "*helm eshell directory history*"
          :resume 'noresume))

  (defun jedahu-eshell-init ()
    (evil-define-operator evil-esh-send-region (beg end)
      (interactive "<r>")
      (save-excursion
        (set-mark beg)
        (goto-char end)
        (eshell-send-input t)))

    (defun jedahu-helm-buffer-names-list ()
      "Preconfigured `helm' to list buffers."
      (interactive)
      (unless helm-source-buffers-list
        (setq helm-source-buffers-list
              (helm-make-source "Buffers" 'helm-source-buffers)))
      (helm :sources '(helm-source-buffers-list
                       helm-source-ido-virtual-buffers
                       helm-source-buffer-not-found)
            :buffer "*helm buffers*"
            :truncate-lines t
            'filtered-candidate-transformer (lambda (candidates sources)
                                              (mapcar #'buffer-name candidates))
            'action (lambda (candidate)
                      (insert "#<buffer " (buffer-name candidate) ">"))))

    (defun jedahu-eshell-complete-at-point ()
      (interactive)
      (if (looking-back "#<buffer ")
          (let* ((len (length "#<buffer "))
                 (beg (- (point) len))
                 (end (point)))
            (with-helm-show-completion beg end
              (jedahu-helm-buffer-names-list)))
        (helm-esh-pcomplete)))

    (spacemacs|define-micro-state eshell-scroll
      :doc "[,] prev [.] next"
      :bindings
      ("," eshell-previous-prompt)
      ("." eshell-next-prompt))

    (define-key eshell-mode-map (kbd "<tab>") 'jedahu-eshell-complete-at-point)

    (evil-leader/set-key-for-mode 'eshell-mode
      (kbd "m RET") 'evil-esh-send-region
      "mgd" 'helm-eshell-cd-history
      "mn" 'spacemacs/eshell-scroll-micro-state)

    (evil-set-initial-state 'eshell-mode 'normal)

    (define-key eshell-mode-map
      [remap eshell-pcomplete] 'helm-esh-pcomplete))

  (add-hook 'eshell-mode-hook 'jedahu-eshell-init)

  (defun process-file-to-string (cmd &rest args)
    (with-output-to-string
      (with-current-buffer
          standard-output
        (apply 'process-file cmd nil t nil args))))

  (defun jedahu-eshell-vcs-info ()
    (let* ((explicit-shell-file-name (if (file-remote-p default-directory)
                                         "/bin/sh"
                                       nil))
           (vcs (ignore-errors (projectile-project-vcs)))
           (branch (case vcs
                     ('git (process-file-to-string "git" "symbolic-ref" "--short" "HEAD"))
                     ('hg (process-file-to-string "hg" "branch"))
                     (t nil))))
      (when branch
        (string-trim (concat (symbol-name vcs) " " branch)))))

  (defun jedahu-eshell-prompt ()
    (let ((header-bg "#fff")
          (vcs-info (jedahu-eshell-vcs-info)))
      (macrolet ((with-face (str &rest properties)
                            `(propertize ,str 'face ',properties)))
        (concat
         (with-face (concat (eshell/pwd) " "))
         (when vcs-info
           (with-face (concat "(" vcs-info ")")
                      :foreground "#888"))
         "\n"
         (with-face user-login-name :foreground "blue")
         "@"
         (with-face "localhost" :foreground "green")
         (if (= (user-uid) 0)
             (with-face " #" :foreground "red")
           " $")
         " "))))

  (defun jedahu-eshell-rename-buffer ()
    (rename-buffer (concat "*eshell*" default-directory) t))

  (add-hook 'eshell-post-command-hook 'evil-normal-state)
  (add-hook 'eshell-post-command-hook 'jedahu-eshell-rename-buffer)

  (setq
   eshell-prompt-function 'jedahu-eshell-prompt
   eshell-highlight-prompt nil))

(defun jedahu/init-eshell ()
  (eval-after-load 'eshell
    '(eval-after-load 'helm
       '(progn
          (jedahu-setup-eshell)
          (jedahu-setup-pcomplete-git)
          ;;(jedahu-setup-pcomplete-hg)
          ))))

(defun jdh-c-mode-common-setup ()
  (setq tab-width 4)
  (setq c-basic-offset tab-width)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'substatement-open 0))

(defun jedahu/init-cc-mode ()
  (add-hook 'c-mode-common-hook 'jdh-c-mode-common-setup))

(defun jedahu/init-eshell-autojump ())

(defun jedahu-helm-setup ()
  (define-key helm-map (kbd "C-z") nil)
  (define-key helm-map (kbd "C-<return>") 'helm-execute-persistent-action))

(defun jedahu/init-helm ()
  (use-package helm
    :defer t
    :commands (helm-find-files-1))
  (eval-after-load 'helm #'jedahu-helm-setup))

(defun jedahu/init-parent-mode ()
  (use-package parent-mode))
