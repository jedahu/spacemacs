;; -*- mode: dotspacemacs -*-

(setq os-mswin? (member system-type '(windows-nt ms-dos cygwin)))

;; (add-to-list 'load-path "~/Documents/p/omnisharp-emacs/")

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-configuration-layer-path nil
   dotspacemacs-configuration-layers
   (append
    '(
      auto-completion
      emacs-lisp
      evil-commentary
      (evil-snipe :variables
                  evil-snipe-enable-alternate-f-and-t-behaviours t
                  evil-snipe-repeat-scope 'whole-buffer)
      evil-surround
      extra-langs
      fsharp
      git
      gnus
      hg
      haskell
      html
      irc
      javascript
      jedahu
      markdown
      org
      pass
      purescript
      restclient
      revealjs
      (shell :variables
             shell-protect-eshell-prompt t)
      shell-scripts
      (slack :variables
             slack-enable-emoji t
             slack-prefer-current-team t)
      smerge
      syntax-checking
      typescript
      typography
      ;; vim-empty-lines
      windows-scripts
      )
    (when os-mswin? '(mswindows)))
   dotspacemacs-excluded-packages
   '(yasnippet
     haskell-yas
     persp-mode
     which-function-mode
     company
     toc-org)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  (setq-default
   dotspacemacs-startup-banner nil
   dotspacemacs-themes '(material
                         spacemacs-dark
                         solarized-dark
                         solarized-light)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "C-SPC"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-,"
   dotspacemacs-command-key ":"
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-persistent-server nil
   dotspacemacs-default-package-repository nil
   dotspacemacs-additional-packages
   '(csharp-mode
     yaml-mode
     purescript-mode
     material-theme
     nodejs-repl
     helm-aws))
  (setq
   ))

(defun dotspacemacs/user-init ()
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq auto-save-default t)
  (setq auto-save-interval 300)
  (setq auto-save-timeout 10)
  (setq auto-save-visited-file-name t)
  (setq-default c-basic-offset 4)
  (setq-default c-syntactic-indentation nil)
  (setq-default c-electric-flag nil)
  (setq compilation-ask-about-save nil)
  (setq create-lockfiles nil)
  (setq epa-file-select-keys nil)
  (setq eshell-prefer-lisp-functions t)
  (setq fsharp-build-command "msbuild")
  (setq git-enable-github-support t)
  (setq gnus-asynchronous t)
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-message-archive-group nil)
  (setq gnus-posting-styles '(((header "to" "jedahu@gmail.com")
                               (address "jedahu@gmail.com"))))
  (setq gnus-read-active-file 'some)
  (setq gnus-secondary-select-methods
        '((nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))))
  (setq jdh--pulp-build "./node_modules/.bin/pulp build")
  (setq jdh--pulp-test "./node_modules/.bin/pulp test")
  (setq jdh--pulp-run "./node_modules/.bin/pulp run")
  (setq markdown-css-path "markdown.css")
  (setq markdown-hr-strings (list
                             (make-string 80 ?-)
                             (string-trim-right
                              (apply 'concat (make-list 40 "- ")))))
  (setq message-directory "~/.gmail")
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mouse-wheel-follow-mouse t)
  (setq mouse-wheel-progressive-speed t)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq nnml-directory "~/.gmail")
  (setq omnisharp-server-executable-path nil)
  (setq org-html-htmlize-output-type 'css)
  (setq org-publish-use-timestamps-flag nil)
  (setq org-src-fontify-natively t)
  (setq powerline-default-separator nil)
  (setq projectile-switch-project-action
        #'(lambda () (dired default-directory)))
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (setq tab-width 4)
  (setq user-mail-address "jedahu@gmail.com")

  (autoload 'gettyped-publish "~/proj/gettyped/org-project.el" nil t)

  (with-eval-after-load 'slack
    (slack-register-team
     :name "takeflite"
     :default t
     :client-id "7738394021.32036010000"
     :client-secret "d619970401a59a3b61fb70f4df042861"
     :token "xoxp-7738394021-7738394037-9680122357-d3f372"
     :subscribed-channels '(general)))

  (when os-mswin?
    (setq
     tramp-default-method "plink"
     ))

  (defmacro ilambda (args &rest body)
    `(lambda ,args
       (interactive)
       ,@body))

  (defun ansi-colorify ()
    (interactive)
    (if (use-region-p)
        (ansi-color-apply-on-region (region-beginning) (region-end))
      (ansi-color-apply-on-region (point-min) (point-max))))

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

  (defun process-file-to-string (cmd &rest args)
    (with-output-to-string
      (with-current-buffer
          standard-output
        (apply 'process-file cmd nil t nil args))))

  (defun save-all ()
    (interactive)
    (save-some-buffers t))

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

  (defun use-dumb-keys ()
    (local-set-key (kbd "RET") 'newline))

  (defun no-fontification ()
    (font-lock-mode -1))

  (add-hook 'focus-out-hook 'save-all)

  (add-to-list 'magic-mode-alist '("diff -r" . diff-mode))

  (define-key evil-evilified-state-map "G" 'evil-goto-line)
  (define-key evil-evilified-state-map "gg" 'evil-goto-first-line)

  (evil-leader/set-key "bU" 'bury-buffer)
  (evil-leader/set-key "br" 'rename-buffer)

  (spacemacs/set-leader-keys
    "p'" 'projectile-run-eshell)

  (with-eval-after-load 'compile
    (pushnew '("^at \\(.*?\\) line \\([0-9]+\\), column \\([0-9]+\\)" 1 2 3)
           compilation-error-regexp-alist))

  (use-package helm
    :defer t
    :config
    (progn
      (define-key helm-map (kbd "C-<return>") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "<S-return>") 'helm-select-action)))

  (use-package markdown-mode
    :defer t
    :config
    (progn
      (add-hook 'markdown-mode-hook 'visual-line-mode)
      (set-face-attribute 'markdown-comment-face nil :strike-through nil)))

  (use-package c-mode
    :defer t
    :config
    (progn
      (defun c-mode-common-setup ()
        (setq tab-width 4)
        (setq c-basic-offset tab-width)
        (c-set-offset 'arglist-intro '++)
        (c-set-offset 'substatement-open 0))
      (add-hook 'c-mode-common-hook 'c-mode-common-setup)))

  (use-package epa-file
    :defer t
    :init (epa-file-enable)
    :config
    (progn
      (setq epa-file-select-keys t)))

  (use-package csharp-mode
    :defer t
    :config
    (progn
      (add-hook 'csharp-mode-hook 'use-dumb-keys)
      (add-hook 'csharp-mode-hook 'no-fontification)))

  (use-package eshell
    :defer t
    :config
    (progn
      (setq helm-eshell-history-map nil)

      (evil-define-operator evil-esh-send-region (beg end)
        (interactive "<r>")
        (save-excursion
          (set-mark beg)
          (goto-char end)
          (eshell-send-input t)))

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

      (setq jedahu-esh-buffers-list nil)

      (defun jedahu-helm-buffer-names-list ()
        "Preconfigured `helm' to list buffers."
        (unless jedahu-esh-buffers-list
          (setq jedahu-esh-buffers-list
                (helm-make-source "Buffers" 'helm-source-buffers))
          (helm-attrset 'action
                        (lambda (x)
                          (message (concat "<<<<<" (type-of x) ">>>>>"))
                          (insert "#<buffer" (buffer-name x) ">"))
                        jedahu-esh-buffers-list))
        (helm :sources '(jedahu-esh-buffers-list
                         helm-source-ido-virtual-buffers
                         helm-source-buffer-not-found)
              :buffer "*helm buffers*"
              :truncate-lines t
              'filtered-candidate-transformer (lambda (candidates sources)
                                                (mapcar #'buffer-name candidates))))

      (defun jedahu-eshell-complete-at-point ()
        (interactive)
        (if (looking-back "#<buffer ")
            (let* ((len (length "#<buffer "))
                   (beg (- (point) len))
                   (end (point)))
              (unwind-protect
                  (with-helm-show-completion beg end
                    (jedahu-helm-buffer-names-list))))
          (helm-esh-pcomplete)))

      (defun jedahu-add-to-eshell-local-map ()
        "Bizarrely, eshell-mode-map is buffer local."
        (define-key eshell-mode-map
          [remap eshell-pcomplete] 'helm-esh-pcomplete)
        (define-key eshell-mode-map
          (kbd "<tab>") 'jedahu-eshell-complete-at-point))

      (spacemacs|define-micro-state eshell-scroll
        :doc "[,] prev [.] next"
        :execute-binding-on-enter t
        :bindings
        ("," eshell-previous-prompt)
        ("." eshell-next-prompt))

      (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
        (kbd "m RET") 'evil-esh-send-region
        "gd" 'helm-eshell-cd-history
        "ib" 'eshell-insert-buffer-name
        "ip" 'eshell-insert-process
        "ie" 'eshell-insert-envvar
        "," 'spacemacs/eshell-scroll-micro-state
        "." 'spacemacs/eshell-scroll-micro-state)

      (add-hook 'eshell-mode-hook 'jedahu-add-to-eshell-local-map)))

  (use-package ispell
    :defer t
    :config
    (progn
      (add-to-list 'ispell-dictionary-alist
                   '(("english"
                      "[[:alpha:]]"
                      "[^[:alpha:]]"
                      "[']"
                      t
                      ("-d" "en_AU")
                      nil
                      utf-8)))
      (setq-default ispell-program-name "hunspell")
      (setq ispell-local-dictionary-alist ispell-dictionary-alist)
      (setq ispell-hunspell-dictionary-alist ispell-dictionary-alist)))

  (use-package ediff
    :defer t
    :config
    (progn
      (defun ediff-copy-both-to-C ()
        (interactive)
        (ediff-copy-diff ediff-current-difference nil 'C nil
                         (concat
                          (ediff-get-region-contents
                           ediff-current-difference 'A ediff-control-buffer)
                          (ediff-get-region-contents
                           ediff-current-difference 'B ediff-control-buffer))))

      (defun add-d-to-ediff-mode-map ()
        (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))

      (add-hook 'ediff-keymap-setup-hook'add-d-to-ediff-mode-map)))

  (server-start))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (slack circe anzu popup tern web-completion-data git-commit spinner package-build tss yaxception nodejs-repl psci deferred psc-ide powerline f hydra markdown-mode multiple-cursors js2-mode projectile smartparens packed avy company-quickhelp haskell-mode yasnippet company gitignore-mode helm helm-core json-reformat csharp-mode auto-complete flycheck magit magit-popup with-editor async s bind-key bind-map evil vi-tilde-fringe persp-mode evil-nerd-commenter yaml-mode xterm-color ws-butler wolfram-mode window-numbering which-key web-mode web-beautify volatile-highlights use-package toc-org tagedit stan-mode spacemacs-theme spaceline solarized-theme smooth-scrolling smeargle slim-mode shut-up shm shell-pop scss-mode scad-mode sass-mode restclient restart-emacs rainbow-delimiters quelpa qml-mode purescript-mode powershell popwin pcre2el pass paradox page-break-lines orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omnisharp neotree multi-term move-text mmm-mode matlab-mode markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme less-css-mode julia-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md fsharp-mode flycheck-purescript flycheck-pos-tip flycheck-haskell flx-ido fish-mode fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-commentary evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help emmet-mode elisp-slime-nav define-word company-web company-tern company-statistics company-ghc company-cabal coffee-mode cmm-mode clean-aindent-mode buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
