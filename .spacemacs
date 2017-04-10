;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
;; * Pre init
(setq os-mswin? (member system-type '(windows-nt ms-dos cygwin)))

(eval-when-compile
  (require 'cl-lib))

;; * dotspacemacs/layers
;; ** Layers
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   (append
    '(
      (auto-completion :variables
                       auto-completion-enable-snippets-in-popup 'manual)
      emacs-lisp
      evil-commentary
      (evil-snipe :variables
                  evil-snipe-enable-alternate-f-and-t-behaviours t
                  evil-snipe-repeat-scope 'whole-buffer)
      extra-langs
      (flow-type :variables
                 flow-type-no-auto-start 'process
                 flow-type-enable-eldoc-type-info nil)
      fsharp
      git
      gnus
      haskell
      html
      javascript
      markdown
      nixos
      org
      pass
      purescript
      restclient
      scala
      (shell :variables
             shell-protect-eshell-prompt t)
      shell-scripts
      (slack :variables
             slack-enable-emoji t
             slack-prefer-current-team t)
      smerge
      sql
      syntax-checking
      typescript
      typography
      ;; vim-empty-lines
      windows-scripts
      )
    (when os-mswin? '()))
   dotspacemacs-install-packages 'used-only

;; ** Exclusions
   dotspacemacs-excluded-packages
   '(haskell-yas
     org-bullets
     smartparens
     which-function-mode
     toc-org)
   ))

;; * dotspacemacs/init
;; ** Init
(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-active-transparency 90
   dotspacemacs-auto-resume-layouts t
   dotspacemacs-auto-save-file-location nil
   dotspacemacs-check-for-update nil
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 8.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-default-package-repository nil
   dotspacemacs-default-package-repository nil
   dotspacemacs-display-default-layout nil
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-emacs-leader-key "C-SPC"
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-ex-command-key ":"
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-folding-method 'evil ;; 'origami
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-resize t
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-highlight-delimiters 'current
   dotspacemacs-inactive-transparency 90
   dotspacemacs-large-file-size 1
   dotspacemacs-leader-key "SPC"
   dotspacemacs-line-numbers nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-major-mode-emacs-leader-key "C-,"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-persistent-server nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-show-transient-state-title t
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-startup-lists '((todos . 5)
                                (recents . 5)
                                (projects . 7)
                                (agenda . 5)
                                (bookmarks . 8))
   dotspacemacs-switch-to-buffer-prefers-purpose t
   dotspacemacs-themes '(spacemacs-dark
                         solarized-dark
                         solarized-light
                         material
                         default
                         )
   dotspacemacs-verbose-loading nil
   dotspacemacs-visual-line-move-text t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-whitespace-cleanup nil

;; ** Packages
   dotspacemacs-additional-packages
   '(
     bnfc
     calfw
     csharp-mode
     dired-narrow
     dizzee
     evil-vimish-fold
     excorporate
     fit-frame
     git-gutter
     helm-aws
     helm-chrome
     kv
     material-theme
     mocha
     nodejs-repl
     ob-http
     org-gcal
     org-tree-slide
     orgtbl-ascii-plot
     outshine
     purescript-mode
     yaml-mode
     ))
  (setq
   ))

;; * dotspacemacs/user-init
(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

;; * dotspacemacs/user-config
;; ** Doc
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

;; ** Require
  (require 'generic-x)
  (require 'cl-lib)

;; ** Setq
;; *** Global
  (setq auto-save-default nil)
  (setq auto-save-interval 300)
  (setq auto-save-timeout 10)
  (setq auto-save-visited-file-name t)
  (setq browse-url-browser-function 'browse-url-default-windows-browser)
  (setq c-basic-offset 4)
  (setq c-syntactic-indentation nil)
  (setq c-electric-flag nil)
  (setq compilation-ask-about-save nil)
  (setq create-lockfiles nil)
  (setq epa-file-select-keys nil)
  (setq eshell-prefer-lisp-functions t)
  (setq evil-fold-list
        '(((outline-minor-mode)
           :open-all outline-show-all
           :close-all jdh-outline-hide-sublevels
           :toggle outline-toggle-children
           :open jdh-outline-show-entry-children
           :open-rec outline-show-subtree
           :close outline-hide-subtree)
          ((hs-minor-mode)
           :open-all hs-show-all
           :close-all hs-hide-all
           :toggle hs-toggle-hiding
           :open hs-show-block
           :open-rec nil
           :close hs-hide-block)
          ((hide-ifdef-mode)
           :open-all show-ifdefs
           :close-all hide-ifdefs
           :toggle nil
           :open show-ifdef-block
           :open-rec nil
           :close hide-ifdef-block)
          ((outline-mode org-mode markdown-mode)
           :open-all show-all
           :close-all #[nil "\300\301!\207" [hide-sublevels 1] 2]
           :toggle outline-toggle-children
           :open #[nil "\300 \210\301 \207" [show-entry show-children] 1]
           :open-rec show-subtree
           :close hide-subtree)
          ((vimish-fold-mode)
           :delete vimish-fold-delete
           :open-all vimish-fold-unfold-all
           :close-all vimish-fold-refold-all
           :toggle vimish-fold-toggle
           :open vimish-fold-unfold
           :open-rec nil
           :close vimish-fold-refold)
          ((origami-mode)
           :open-all #[nil "\300p!\207" [origami-open-all-nodes] 2]
           :close-all #[nil "\300p!\207" [origami-close-all-nodes] 2]
           :toggle #[nil "\300p`\"\207" [origami-toggle-node] 3]
           :open #[nil "\300p`\"\207" [origami-open-node] 3]
           :open-rec #[nil "\300p`\"\207" [origami-open-node-recursively] 3]
           :close #[nil "\300p`\"\207" [origami-close-node] 3])))
  (setq excorporate-configuration '("jeremy.hughes@arlo.co" . "https://outlook.office365.com/ews/services.wsdl"))
  (setq explicit-bash-args '("--noediting" "--rcfile" "~/.bashrc_emacs" "-i"))
  (setq flow-executable "flow")
  (setq flow-common-args '("--quiet" "--no-auto-start" "--show-all-errors"))
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-error-list-minimum-level 'error)
  (setq flycheck-javascript-flow-args '("--no-auto-start"))
  (setq flycheck-javascript-flow-executable flow-executable)
  (setq flycheck-pos-tip-timeout 999)
  (setq flycheck-standard-error-navigation t)
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
  (setq helm-follow-mode-persistent nil)
  (setq holy-mode nil)
  (setq jdh--pulp-build "./node_modules/.bin/pulp build")
  (setq jdh--pulp-test "./node_modules/.bin/pulp test")
  (setq jdh--pulp-run "./node_modules/.bin/pulp run")
  (setq jdh-markdown-hide-inline-markers nil)
  (setq js--prettify-symbols-alist
        '(("=>" . ?⇒)
          (">=" . ?≥)
          ("<=" . ?≤)
          ("===" . ?≡)
          ("&&" . ?∧)
          ("||" . ?∨)
          ("void" . ?⊥)))
  (setq js-doc-parameter-line " * @arg {} %p\n")
  (setq js-doc-return-line " * @returns {}\n")
  (setq js-doc-throw-regexp "throw\\|raise")
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
  (setq org-agenda-files "~/AGENDA")
  (setq org-agenda-include-diary t)
  (setq org-babel-default-header-args:sh
        '((:prologue . "exec 2>&1")
             (:epilogue . ":")
             (:results . "output verbatim")
             (:wrap . "ANSI")))
  (setq org-bullets-mode nil)
  ;; (setq org-confirm-babel-evaluate '(lambda (lang _body)
  ;;                                     (not (member lang '("gnuplot" "ditaa")))))
  (setq org-confirm-babel-evaluate nil)
  (setq org-descriptive-links t)
  (setq org-gcal-client-id "1094137197380-a6epgtjno65j4dc4t5841u9q12t0q5qt.apps.googleusercontent.com")
  (setq org-gcal-client-secret "h4p__gunYsHwgb7sJj21ZxKy")
  (setq org-gcal-file-alist '(("jeremy.hughes@learningsourcehq.com" . "~/CALENDAR.org")))
  (setq org-hide-block-overlays t)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-inline-src-markers t)
  (setq org-hide-leading-starts nil)
  (setq org-hide-macro-markers t)
  (setq org-html-head "<link rel=stylesheet type=text/css href=org-info.css>")
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-use-infojs t)
  (setq org-html-infojs-options
        '((path . "org-info.js")
          (view . "showall")
          (toc . "nil")
          (ftoc . "0")
          (tdepth . "max")
          (sdepth . "max")
          (mouse . "#eeeeee")
          (buttons . "0")
          (ltoc . "nil")
          (up . :html-link-up)
          (home . :html-link-home)))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-open-directory-means-index-dot-org nil)
  (setq org-pomodoro-format ":%s")
  (setq org-pomodoro-time-format "%.2m")
  (setq org-publish-use-timestamps-flag nil)
  (setq org-src-fontify-natively t)
  (setq org-tags-column -80)
  (setq persp-kill-foreign-buffer-behaviour 'kill)
  (setq powerline-default-separator 'utf-8)
  (setq powerline-utf-8-separator-left 124)
  (setq powerline-utf-8-separator-right 124)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'projectile-run-eshell)
  (setq purpose-user-mode-purposes '((web-mode . edit)))
  (setq shell-default-shell 'shell)
  (setq shr-external-browser 'browse-url-xdg-open)
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (setq spacemacs-theme-org-height nil)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq tab-width 4)
  (setq user-mail-address "jedahu@gmail.com")
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;; *** Conditional
  (when os-mswin?
    (setq org-pomodoro-audio-player "sounder")
    (setq tramp-default-method "plink")
    )

;; ** Global modes
  (evil-vimish-fold-mode 1)
  (yas-global-mode 1)
  (global-eldoc-mode -1)
  (spacemacs/toggle-fill-column-indicator-on)
  (pupo-mode -1)
  (global-flycheck-mode -1)

;; ** File types
  (add-to-list 'auto-mode-alist '("\\.es\\.flow\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.es\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\.flow\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("/\\.babelrc\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.flowconfig\\'" . ini-generic-mode))

;; ** Functions
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

  (defun jdh-file-string (path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))

  (defun jdh-file-lines (path)
    (split-string (jdh-file-string path) "\n" t))

  (defun jdh-process-string (cmd &rest args)
    (with-temp-buffer
      (apply 'call-process cmd nil (current-buffer) nil args)
      (buffer-string)))

  (defun jdh--fontify-ansi-colors (limit)
    (ansi-color-apply-on-region (point) limit))

  (defun jdh-fontify-region-or-buffer ()
    (if (region-active-p)
        (font-lock-fontify-region)
      (font-lock-fontify-buffer)))

  (defun jdh-outline-hide-sublevels ()
    (interactive)
    (outline-hide-sublevels 2))

  (defun jdh-outline-show-entry-children ()
    (interactive)
    (outline-show-entry)
    (outline-show-children))

  (defun jdh-goto-dominating-todo ()
    (interactive)
    (if-let (dir (locate-dominating-file default-directory "TODO.org"))
        (find-file (f-join dir "TODO.org"))
      (message "No TODO.org found.")))

  (defun jdh-copy-visible (keepp)
    "Create a copy of the visible part of the current buffer and add
it to the kill ring so it can be copied into other buffers or programs.
The copy is created in a temporary buffer and removed after use.
As a special case, if you have a prefix arg KEEPP, the temporary
buffer will not be removed but presented to you so that you can
continue to use it.
This function is derived from org-export-visible."
    (interactive "P")
    (let* ((file buffer-file-name)
           (buffer (get-buffer-create "*Copy Visible*"))
           s e)
      (with-current-buffer buffer (erase-buffer))
      (save-excursion
        (setq s (goto-char (point-min)))
        (while (not (= (point) (point-max)))
          (goto-char (jdh-find-invisible))
          (append-to-buffer buffer s (point))
          (setq s (goto-char (jdh-find-visible))))
        (goto-char (point-min))
        (set-buffer buffer)
        (kill-new (buffer-substring (point-min) (point-max)))
        (if (not keepp)
            (kill-buffer buffer)
          (switch-to-buffer-other-window buffer)
          (goto-char (point-min))))))

  (defun jdh-find-visible ()
    (let ((s (point)))
      (while (and (not (= (point-max) (setq s (next-overlay-change s))))
                  (get-char-property s 'invisible)))
      s))

  (defun jdh-find-invisible ()
    (let ((s (point)))
      (while (and (not (= (point-max) (setq s (next-overlay-change s))))
                  (not (get-char-property s 'invisible))))
      s))

;; ** Services

;; ** Modes
  (define-derived-mode ansi-mode fundamental-mode "ansi"
    "Fundamental mode that understands ANSI colors."
    (require 'ansi-color)
    (font-lock-add-keywords nil '((jdh--fontify-ansi-colors))))

;; ** Lists
  (add-to-list 'magic-mode-alist '("diff -r" . diff-mode))

  (add-to-list 'evil-fold-list
               '((hs-minor-mode)
                 :open-all hs-show-all
                 :close-all hs-hide-all
                 :toggle hs-toggle-hiding
                 :open hs-show-block
                 :open-rec nil
                 :close hs-hide-block))

  (add-to-list 'evil-fold-list
               '((outline-minor-mode)
                 :open-all outline-show-all
                 :close-all jdh-outline-hide-sublevels
                 :toggle outline-toggle-children
                 :open jdh-outline-show-entry-children
                 :open-rec outline-show-subtree
                 :close outline-hide-subtree))

  (add-to-list 'compilation-error-regexp-alist-alist
               '(js-runtime "\\(?:^[\t ]*at \\|(\\)\\(\\(?:[A-Za-z]:\\)?[^:()\n]+\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:)\\|$\\)" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'js-runtime)

;; *** Hooks
  (add-hook 'focus-out-hook 'save-all)
  ;; (add-hook 'evil-normal-state-entry-hook 'save-buffer)
  (add-hook 'before-save-hook 'time-stamp)

;; ** Bindings
  (define-key evil-evilified-state-map "G" 'evil-goto-line)
  (define-key evil-evilified-state-map "gg" 'evil-goto-first-line)
  (define-key evil-normal-state-map "zf" 'evil-vimish-fold/create)
  (define-key evil-normal-state-map "'" 'helm-evil-markers)

  (evil-leader/set-key "aoc" nil)
  (evil-leader/set-key "aoC" 'org-capture)
  (evil-leader/set-key "aocg" 'org-clock-goto)
  (evil-leader/set-key "aoct" 'org-clock-select-task)
  (evil-leader/set-key "aoco" 'org-clock-out)
  (evil-leader/set-key "br" 'rename-buffer)
  (evil-leader/set-key "bU" 'bury-buffer)
  (evil-leader/set-key "e," 'flycheck-display-error-at-point)

  (evil-leader/set-key "xts" 'transpose-sexps)

  (evil-define-key 'normal sh-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer)

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer)

  (evil-define-key 'normal js-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer)

  (evil-define-key 'normal js2-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer)

  (evil-define-minor-mode-key 'normal 'outline-minor-mode
    (kbd "<backtab>") 'outshine-cycle-buffer
    "zj" 'outline-next-heading
    "zk" 'outline-previous-heading)

  (evil-define-key 'normal org-mode-map
    "zj" 'org-next-visible-heading
    "zk" 'org-previous-visible-heading)

  (spacemacs/set-leader-keys
    "p'" 'projectile-run-eshell
    "po" 'jdh-goto-dominating-todo)

;; ** Modules

;; *** Elisp

  (defun jdh--emacs-lisp-mode-setup ()
    (setq-local tab-width 8))

  (add-hook 'emacs-lisp-mode-hook 'jdh--emacs-lisp-mode-setup)

;; *** ssh
  (defun jdh-ssh-agent-start ()
    (with-current-buffer (get-buffer-create "*ensure-ssh*")
      (erase-buffer)
      (call-process "ssh-agent" nil t)
      (goto-char 0)
      (search-forward "SSH_AUTH_SOCK=")
      (replace-match "")
      (search-forward "; export SSH_AUTH_SOCK")
      (replace-match "")
      (search-forward "SSH_AGENT_PID=")
      (replace-match "")
      (search-forward "; export SSH_AGENT_PID")
      (write-file "~/.ssh/agent.env")))

  (defun jdh-ssh-load-env ()
    (when (file-readable-p "~/.ssh/agent.env")
      (let ((lines (jdh-file-lines "~/.ssh/agent.env")))
        (setenv "SSH_ASKPASS"
                (concat (string-trim (jdh-process-string "git" "--exec-path"))
                        "/git-gui--askpass"))
        (setenv "SSH_AUTH_SOCK" (nth 0 lines))
        (setenv "SSH_AGENT_PID" (nth 1 lines)))))

  (defun jdh-ssh-agent-ensure ()
    (interactive)
    (jdh-ssh-load-env)
    (let ((exit (call-process "ssh-add" nil nil nil "-l")))
      (case exit
        (1 (call-process "ssh-add" nil nil nil "~/.ssh/id_rsa"))
        (2 (jdh-ssh-agent-start)
           (jdh-ssh-load-env)
           (call-process "ssh-add" nil nil nil "~/.ssh/id_rsa")))))

  (jdh-ssh-agent-ensure)

;; *** C
  (with-eval-after-load 'c-mode
    (defun c-mode-common-setup ()
      (setq tab-width 4)
      (setq c-basic-offset tab-width)
      (c-set-offset 'arglist-intro '++)
      (c-set-offset 'substatement-open 0))
    (add-hook 'c-mode-common-hook 'c-mode-common-setup))

;; *** ediff
  (with-eval-after-load 'ediff
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

    (add-hook 'ediff-keymap-setup-hook'add-d-to-ediff-mode-map))

;; *** EPA
  (use-package epa-file
    :defer t
    :init (epa-file-enable)
    :config
    (progn
      (setq epa-file-select-keys t)))

;; *** eshell
  (with-eval-after-load 'eshell
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
      "." 'spacemacs/eshell-scroll-micro-state))

;; *** flycheck
  (with-eval-after-load 'flycheck
    (defun jdh--flycheck-eslint-to-info (errs)
      (dolist (e errs)
        (when (eq 'javascript-eslint (flycheck-error-checker e))
          (setf (flycheck-error-level e) 'info)))
      errs)

    (defun jdh--flycheck-flow-coverage-region (fun err mode)
      (if (and (eq 'javascript-flow-coverage (flycheck-error-checker err))
               (member mode '(symbols sexps)))
          (flycheck-error-with-buffer err
            (save-restriction
              (save-excursion
                (widen)
                (let* ((beg (progn
                              (goto-char (point-min))
                              (forward-line (1- (flycheck-error-line err)))
                              (move-to-column (1- (flycheck-error-column err)))
                              (point)))
                       (to (cadr (read (format "(%s)" (flycheck-error-message err)))))
                       (end (progn
                              (goto-char (point-min))
                              (forward-line (1- (car to)))
                              (move-to-column (cdr to))
                              (point))))
                  (cons beg end)))))
        (funcall fun err mode)))

    ;; (advice-add 'flycheck-parse-checkstyle :filter-return 'jdh--flycheck-eslint-to-info)

    (advice-add 'flycheck-error-region-for-mode :around 'jdh--flycheck-flow-coverage-region)

    (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
    (flycheck-add-next-checker 'javascript-flow-coverage 'javascript-eslint)
    )

;; *** Helm
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-<return>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "<S-return>") 'helm-select-action)

    (defvar jdh--helm-evil-markers-candidate nil)

    (defun jdh--helm-evil-goto-mark-line ()
      (interactive)
      (let* ((candidates (gethash "evil-markers" helm-candidate-cache))
             (keys (recent-keys))
             (k (elt keys (- (length keys) 1)))
             (candidate (assoc-default k candidates (lambda (x y) (eq y (string-to-char x))))))
        (setq jdh--helm-evil-markers-candidate candidate)
        (helm-exit-and-execute-action
         (lambda (_)
           (switch-to-buffer (marker-buffer jdh--helm-evil-markers-candidate))
           (goto-char (marker-position jdh--helm-evil-markers-candidate))))))

    (setq helm-evil-markers-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map helm-map)
        (dolist (k (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
          (define-key map (string k) 'jdh--helm-evil-goto-mark-line))
        (delq nil map)))

    (defun helm-evil-markers ()
      (interactive)
      (helm :sources (helm-build-sync-source "evil-markers"
                       :candidates
                       (mapcar (lambda (x)
                                 (destructuring-bind (k . v) x
                                   (let* ((m (if (consp v)
                                                 (set-marker (make-marker) (cdr v) (or (find-buffer-visiting (car v))
                                                                                       (find-file-noselect (car v))))
                                               v))
                                          (buf (marker-buffer m)))
                                     (cons
                                      (concat (string k) " "
                                              (buffer-name buf) " "
                                              (with-current-buffer buf
                                                (save-excursion
                                                  (goto-char (marker-position m))
                                                  (buffer-substring (point-at-bol) (point-at-eol)))))
                                      m))))
                               (map-filter (lambda (k v)
                                             (or (markerp v) (and (consp v) (not (eq 'lambda (car v))))))
                                           (default-value 'evil-markers-alist)))
                       :action '(("Visit" . (lambda (candidate)
                                              (switch-to-buffer (marker-buffer candidate))
                                              (goto-char (marker-position candidate)))))
                       :persistent-action (lambda (candidate)
                                            (helm-switch-to-buffers (marker-buffer candidate))
                                            (helm-goto-char (marker-position candidate))
                                            (helm-highlight-current-line))
                       :fuzzy-match t
                       :keymap 'helm-evil-markers-map)
            :resume 'noresume
            :buffer "*helm evil-markers*"
            ))
    )

;; *** ispell
  (with-eval-after-load 'ispell
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
    (setq ispell-hunspell-dictionary-alist ispell-dictionary-alist))

;; *** javascript
  (with-eval-after-load 'js2-mode
    (defun jdh--js-find-imports ()
      (let ((case-fold-search t))
        (search-backward-regexp
         (format "^%s%s[[:space:]]*?$"
                 (regexp-quote comment-start)
                 (comment-padleft "[*]+ imports"))
         nil)))

    (defun jdh-js-insert-import (line)
      (interactive "*Mimport ")
      (save-excursion
        (save-restriction
          (jdh--js-find-imports)
          (forward-line 1)
          (let ((beg (point)))
            (insert "import " line "\n")
            (search-forward-regexp "^[[:space:]]*?$" nil 'noerror)
            (forward-line -1)
            (sort-lines nil beg (point-at-eol))))))

    (defun jdh-js-sort-imports ()
      (interactive)
      (save-excursion
        (save-restriction
          (jdh--js-find-imports)
          (forward-line 1)
          (let ((beg (point)))
            (search-forward-regexp "^[[:space:]]*?$" nil 'noerror)
            (forward-line -1)
            (sort-lines nil beg (point-at-eol))))))

    (defun jdh--js2-mode-setup ()
      (setq mode-name "JS2")
      (spacemacs/toggle-fill-column-indicator-on)
      (setq-local comment-start "//")
      (setq-local comment-empty-lines t)
      (eldoc-mode -1)
      (company-mode -1)
      (if jdh--outorg-in-edit-p
            (setq org-descriptive-links nil)
        (flycheck-mode 1))
      )

    (add-hook 'js2-mode-hook 'jdh--js2-mode-setup)

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "ii" 'jdh-js-insert-import
      "is" 'jdh-js-sort-imports)
    ;; (defun jdh--js-mode-setup ()
    ;;   (spacemacs/toggle-auto-completion-on)
    ;;   (outline-minor-mode 1)
    ;;   (save-excursion
    ;;     (goto-char 0)
    ;;     (when (ignore-errors (re-search-forward "^// *@flow\\>"))
    ;;       (setq company-backends '(company-yasnippet company-flow)))))

    ;; (defun jdh--js2-mode-setup ()
    ;;   (js2-mode-hide-warnings-and-errors))

    ;; (evil-define-key 'insert js-mode-map
    ;;   "\t" 'company-indent-or-complete-common)

    ;; (evil-define-key 'insert js2-mode-map
    ;;   "\t" 'company-indent-or-complete-common)

    ;; (add-hook 'js-mode-hook 'jdh--js-mode-setup t)
    ;; (add-hook 'js2-mode-hook 'jdh--js-mode-setup t)
    ;; (add-hook 'js2-mode-hook 'jdh--js2-mode-setup t)

;; **** overrides
    (defun jdh--js--proper-indentation (_ parse-status)
      "Return the proper indentation for the current line."
      (save-excursion
        (back-to-indentation)
        (cond ((nth 4 parse-status)    ; inside comment
               (js--get-c-offset 'c (nth 8 parse-status)))
              ((nth 3 parse-status) 0) ; inside string
              ((eq (char-after) ?#) 0)
              ((save-excursion (js--beginning-of-macro)) 4)
              ;; Indent array comprehension continuation lines specially.
              ((let ((bracket (nth 1 parse-status))
                     beg)
                 (and bracket
                      (not (js--same-line bracket))
                      (setq beg (js--indent-in-array-comp bracket))
                      ;; At or after the first loop?
                      (>= (point) beg)
                      (js--array-comp-indentation bracket beg))))
              ((js--ctrl-statement-indentation))
              ((js--multi-line-declaration-indentation))
              ((nth 1 parse-status)
               ;; A single closing paren/bracket should be indented at the
               ;; same level as the opening statement. Same goes for
               ;; "case" and "default".
               (let ((same-indent-p (looking-at "[]})]\\||}")) ;; JDH modified regex
                     (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                     (continued-expr-p (js--continued-expression-p)))
                 (goto-char (nth 1 parse-status)) ; go to the opening char
                 (if (looking-at "\\([({[]\\|{|\\)\\s-*\\(/[/*]\\|$\\)") ;; JDH modified regex
                     (progn ; nothing following the opening paren/bracket
                       (skip-syntax-backward " ")
                       (when (eq (char-before) ?\)) (backward-list))
                       (back-to-indentation)
                       (js--maybe-goto-declaration-keyword-end parse-status)
                       (let* ((in-switch-p (unless same-indent-p
                                             (looking-at "\\_<switch\\_>")))
                              (same-indent-p (or same-indent-p
                                                 (and switch-keyword-p
                                                      in-switch-p)))
                              (indent
                               (cond (same-indent-p
                                      (current-column))
                                     (continued-expr-p
                                      (+ (current-column) (* 2 js-indent-level)
                                         js-expr-indent-offset))
                                     (t
                                      (+ (current-column) js-indent-level
                                         (pcase (char-after (nth 1 parse-status))
                                           (?\( js-paren-indent-offset)
                                           (?\[ js-square-indent-offset)
                                           (?\{ js-curly-indent-offset)))))))
                         (if in-switch-p
                             (+ indent js-switch-indent-offset)
                           indent)))
                   ;; If there is something following the opening
                   ;; paren/bracket, everything else should be indented at
                   ;; the same level.
                   (unless same-indent-p
                     (forward-char)
                     (skip-chars-forward " \t"))
                   (current-column))))

              ((js--continued-expression-p)
               (+ js-indent-level js-expr-indent-offset))
              (t 0))))

    (defun jdh--js--looking-at-operator-p (_)
      "Return non-nil if point is on a JavaScript operator, other than a comma."
      (save-match-data
        (and (looking-at js--indent-operator-re)
             (not (and (looking-at "|") (eq ?\{ (char-before)))) ;; JDH added
             (or (not (eq (char-after) ?:))
                 (save-excursion
                   (and (js--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                        (eq (char-after) ??))))
             (not (and
                   (eq (char-after) ?*)
                   ;; Generator method (possibly using computed property).
                   (looking-at (concat "\\* *\\(?:\\[\\|" js--name-re " *(\\)"))
                   (save-excursion
                     (js--backward-syntactic-ws)
                     ;; We might misindent some expressions that would
                     ;; return NaN anyway.  Shouldn't be a problem.
                     (memq (char-before) '(?, ?} ?{))))))))

    (advice-add 'js--proper-indentation :around #'jdh--js--proper-indentation)
    (advice-add 'js--looking-at-operator-p :around #'jdh--js--looking-at-operator-p)
    )

;; *** Markdown
  (with-eval-after-load 'markdown-mode

    (defun jdh--markdown-fontify-links (limit)
      "Fontify inline src."
      (when (re-search-forward "\\(\\[\\)\\(.+?\\)\\(\\]\\)\\((.+?)\\)" limit t)
        ;; (add-text-properties
        ;;  (match-beginning 0) (match-end 0)
        ;;  '(font-lock-fontified t face org-code))
        (when jdh-markdown-hide-inline-markers
          (add-text-properties (match-beginning 4) (match-end 4)
                               '(invisible t))
          (add-text-properties (match-beginning 3) (match-end 3)
                               '(invisible t))
          (add-text-properties (match-beginning 1) (match-end 1)
                               '(invisible t)))
        ;; (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
        t))

    (defun jdh--markdown-fixed-height-headings ()
      (dolist (face '(markdown-header-face
                      markdown-header-face-1
                      markdown-header-face-2
                      markdown-header-face-3
                      markdown-header-face-4
                      markdown-header-face-5
                      markdown-header-face-6))
        (set-face-attribute face nil :height 1.0)))

    (defun jdh--setup-markdown ()
      (add-to-list 'font-lock-extra-managed-props 'invisible)
      (jdh--markdown-fixed-height-headings)
      (font-lock-add-keywords nil
                              '((jdh--markdown-fontify-links))
                              t))

    (add-hook 'markdown-mode-hook 'jdh--setup-markdown)
    (set-face-attribute 'markdown-comment-face nil :strike-through nil))

;; *** Misc
  ;; (with-eval-after-load 'compile
  ;;   (add-to-list 'compilation-error-regexp-alist-alist
  ;;                '(flow "^\\([^:\n]+\\):\\([0-9]+\\)$" 1 2))
  ;;   (setq compilation-error-regexp-alist '(flow))
  ;;   ;; (pushnew '("^at \\(.*?\\) line \\([0-9]+\\), column \\([0-9]+\\)" 1 2 3)
  ;;   ;;          compilation-error-regexp-alist)
  ;;   )

  (with-eval-after-load 'lisp-mode
    (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))

  (with-eval-after-load 'js
    (add-hook 'js-mode-hook 'outline-minor-mode))

  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook 'outline-minor-mode))

  (with-eval-after-load 'shell
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

  (with-eval-after-load 'sh-script
    (add-hook 'sh-mode-hook 'outline-minor-mode))

  (with-eval-after-load 'outline
    (require 'outshine)
    (diminish 'outline-minor-mode)
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

;; *** nix-mode
  (with-eval-after-load 'nix-mode
    (add-hook 'nix-mode-hook 'hs-minor-mode))

;; *** Org
  (with-eval-after-load 'org
    (require 'org-agenda)
    (require 'org-collector)

    (dolist (x '(((prefix . "tfs") (envvar . "TFS_WORKITEM_URL"))
                 ((prefix . "zen") (envvar . "ZENDESK_TICKET_URL"))))
      (lexical-let ((x-url (getenv (cdr (assoc 'envvar x))))
                    (x-pfx (cdr (assoc 'prefix x))))
        (when x-url
          (org-link-set-parameters
           x-pfx
           :follow (lambda (path)
                     (org-open-link-from-string (concat x-url path)))
           :export (lambda (path desc backend)
                     (message "BACKEND: %s" backend)
                     (case backend
                       (html
                        (format "<a href='%s'>%s</a>"
                                (concat x-url path)
                                (or desc (format "%s:%s" x-pfx path))))
                       ))))))

    (defun jdh-org-html--format-image-data-uri (fn src attrs info)
      (let ((uri (with-temp-buffer
                   (insert-file-contents src)
                   (base64-encode-region (point-min) (point-max))
                   (goto-char 1)
                   (insert "data:;base64,")
                   (buffer-string))))
        (funcall fn uri attrs info)))

    (defun jdh-org-update-velocity ()
      (interactive)
      (save-excursion
        (org-back-to-heading t)
        (let* ((est-string (org-entry-get (point) "Effort"))
               (time (org-clock-sum-current-item)))
          (when (and est-string (org-entry-is-done-p) time (< 0 time))
            (org-entry-put
             (point)
             "Actual"
             (format "%s" (/ (round (* 10.0 (/ (float time) 60))) 10.0)))
            (org-back-to-heading t)
            (org-entry-put
             (point)
             "Velocity"
             (format "%s"
                     (/
                      (round
                       (* 10.0
                          (/ (* 60.0 (string-to-number est-string))
                             (float time))))
                      10.0)))))))

    (defun jdh-org-insert-olp (path &optional sort fn top)
      (let ((start (point)))
        (if top
            (progn (goto-char (point-max))
                   (org-insert-heading nil nil 'top))
          (org-insert-subheading '(4)))
        (insert (car path))
        (dolist (p (cdr path))
          (org-insert-subheading '(4))
          (insert p))
        (if fn (funcall fn) (newline))
        (when sort
          (goto-char start)
          (apply #'org-sort-entries sort))))

    (defun jdh-org-ensure-olp (path &optional overflow sort fn)
      (if-let (m (and path (ignore-errors (org-find-olp path 'this-buffer))))
          (progn
            (goto-char (marker-position m))
            (if overflow
                (jdh-org-insert-olp overflow sort fn)
              (when fn (funcall fn))))
        (if path
            (jdh-org-ensure-olp
             (butlast path) (cons (car (last path)) overflow) sort fn)
          (if overflow
              (jdh-org-insert-olp overflow sort fn 'top)
            (when fn (funcall fn))))))

    (defun jdh--org-fontify-inline-src (limit)
      "Fontify inline src."
      (when (re-search-forward "\\(\\<src_[a-z]+?{\\)[^{]+?\\(}\\)" limit t)
        (add-text-properties
         (match-beginning 0) (match-end 0)
         '(font-lock-fontified t face org-code))
        (when org-hide-inline-src-markers
         (add-text-properties (match-beginning 2) (match-end 2)
                              '(invisible t))
         (add-text-properties (match-beginning 1) (match-end 1)
                              '(invisible t)))
        (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
        t))

    (defun jdh--org-fontify-ansi-block (limit)
      (when (re-search-forward "^[ \t]*#\\+BEGIN_ANSI[ \t]*$" limit t)
        (message "begin")
        (let ((start (match-end 0)))
          (when (re-search-forward "^[ \t]*#\\+END_ANSI[ \t]*$" nil t)
            (message "end")
            (let ((end (match-beginning 0)))
              (ansi-color-apply-on-region start end)
              (add-text-properties
               start end
               '(font-lock-fontified t font-lock-multiline t))
              t)))))

    (defun jdh--org-fontify-ansi-src (limit)
      (when (re-search-forward "^[ \t]*#\\+BEGIN_SRC[ \t]+ansi[ \t]*$" limit t)
        (message "begin")
        (let ((start (match-end 0)))
          (when (re-search-forward "^[ \t]*#\\+END_SRC[ \t]*$" nil t)
            (message "end")
            (let ((end (match-beginning 0)))
              (ansi-color-apply-on-region start end)
              (add-text-properties
               start end
               '(font-lock-fontified t font-lock-multiline t))
              t)))))

    (defun jdh--org-mode-setup ()
      (setq-local time-stamp-start "^#\\+DATE:[ \t]+[<\[]")
      (setq-local time-stamp-end ">\\|\\]")
      (setq-local time-stamp-format "%:y-%02m-%02d %03a")
      (org-indent-mode -1)
      (org-add-link-type "doc"
                         (lambda (path)
                           (find-file path)))
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (sh . t)
         (gnuplot . t)
         ))
      (dolist (face '(org-document-title
                      org-level-1
                      org-level-2
                      org-level-3
                      org-level-4
                      org-level-5
                      org-level-6
                      org-level-7
                      org-level-8))
        (set-face-attribute face nil :height 1.0))
      (font-lock-add-keywords nil
                              '((jdh--org-fontify-inline-src)
                                (jdh--org-fontify-ansi-block)
                                (jdh--org-fontify-ansi-src))
                              t)
      (when jdh--outorg-in-edit-p
        (setq fill-column 77)
        (setq org-hide-macro-markers nil)
        (setq org-hide-emphasis-markers nil)
        (jdh-fontify-region-or-buffer))
      )

    (defun jdh-org-toggle-macro-markup ()
      (interactive)
      (setq org-hide-macro-markers (not org-hide-macro-markers))
      (jdh-fontify-region-or-buffer))

    (defun jdh-org-toggle-emphasis-markup ()
      (interactive)
      (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
      (jdh-fontify-region-or-buffer))

    (defun jdh-org-toggle-inline-src-markup ()
      (interactive)
      (setq org-hide-inline-src-markers (not org-hide-inline-src-markers))
      (jdh-fontify-region-or-buffer))

    (defun jdh--org-maybe-export ()
      (when (and
             (eq major-mode 'org-mode)
             (save-excursion
               (goto-char 0)
               (re-search-forward "^#\\+AUTOEXPORT\\b" nil t)))
        (org-html-export-to-html)))

    (defun jdh--org-update-blocks ()
      (when (eq major-mode 'org-mode)
        (org-update-all-dblocks)))

    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "occ" 'org-columns
      "ocq" 'org-columns-quit
      "otm" 'jdh-org-toggle-macro-markup
      "ote" 'jdh-org-toggle-emphasis-markup
      "ots" 'jdh-org-toggle-inline-src-markup
      "B" 'org-tree-to-indirect-buffer
      "b" nil
      "bx" 'org-babel-execute-src-block
      "br" 'org-babel-remove-result-one-or-many
      "bt" (ilambda () (org-babel-tangle '(4)))
      "bp" 'org-babel-pop-to-session-maybe)

    (add-hook 'org-mode-hook 'jdh--org-mode-setup)
    (add-hook 'before-save-hook 'jdh--org-update-blocks)
    (add-hook 'after-save-hook 'jdh--org-maybe-export)
    (add-hook 'org-clock-out-hook 'jdh-org-update-velocity)
    (add-hook 'org-shiftup-hook 'jdh-org-update-velocity)
    (add-hook 'org-shiftdown-hook 'jdh-org-update-velocity)

    (advice-add 'org-html--format-image
                :around 'jdh-org-html--format-image-data-uri)
    )

;; **** org vsts
  (with-eval-after-load 'org
    (require 'request)

    (defvar jdh-vsts-url nil)
    (defvar jdh-vsts-auth nil)

    (defun jdh-vsts-get-workitem (id fn)
      (interactive)
      (lexical-let ((f fn))
        (request
         (format "%s/_apis/wit/workitems" jdh-vsts-url)
         :type "GET"
         :params `(("api-version" . "1.0")
                   ("ids" . ,id))
         :headers `(("Authorization" . ,(format "Basic %s" jdh-vsts-auth)))
         :parser 'json-read
         :error (cl-function
                 (lambda (&rest args)
                   (insert "Error fetching work item")))
         :success (cl-function
                   (lambda (&key data &allow-other-keys)
                     (funcall f data))))))

    (defun jdh-org-vsts-update-workitem ()
      (save-excursion
        (org-back-to-heading)
        (lexical-let ((buf (current-buffer)))
          (when-let (wid (org-entry-get (point) "VSTS_WORKITEM_ID"))
            (message (format "Fetching WID %s" wid))
            (jdh-vsts-get-workitem
             wid
             #'(lambda (data)
                 (with-current-buffer buf
                   (let* ((elem (org-element-at-point))
                          (end (plist-get (second elem) :end))
                          (fields
                           (assoc-default
                            'fields
                            (aref (assoc-default 'value data) 0)))
                          (descr (assoc-default 'System.Description fields))
                          (crit (assoc-default 'Microsoft.VSTS.Common.AcceptanceCriteria fields)))

                     (when (search-forward-regexp
                            "^:DESCRIPTION:[ \t]*\n\\(?:[^\n]*\n\\)*?:END:[ \t]*?$"
                            end 'noerror)
                       (replace-match ""))
                     (shr-ensure-newline)
                     (insert ":DESCRIPTION:\n")
                     (let ((descr-start (point)))
                       (insert (format "%s" descr))
                       (shr-render-region descr-start (point)))
                     (if (looking-at "^[[:space:]]*$")
                       (delete-region (point) (point-at-eol))
                       (insert "\n"))
                     (insert ":END:")

                     (org-back-to-heading)
                     (let ((elem (org-element-at-point))
                           (end (plist-get (second elem) :end)))
                       (when (search-forward-regexp
                              "^:ACCEPTANCE_CRITERIA:[ \t]*\n\\(?:[^\n]*\n\\)*?:END:[ \t]*?$"
                              end 'noerror)
                         (replace-match ""))

                       (shr-ensure-newline)
                       (insert ":ACCEPTANCE_CRITERIA:\n")
                       (let ((crit-start (point)))
                         (insert (format "%s" crit))
                         (shr-render-region crit-start (point)))
                       (shr-ensure-newline)
                       (insert ":END:"))

                     ))))))))
    )

;; **** org-projectile
  (with-eval-after-load 'org-projectile
    (defun jdh--projectile-relative-file-name (file)
      (concat "./" (file-relative-name file (projectile-project-root))))

    (defun jdh--todo-relative-path (path)
      (f-relative path (locate-dominating-file path "TODO.org")))

    (defun jdh--find-todo-location ()
      (let* ((dir (locate-dominating-file buffer-file-name "TODO.org"))
             (todo (f-join dir "TODO.org"))
             (text (buffer-substring-no-properties (point) (mark)))
             (path (split-string
                    (f-relative buffer-file-name dir)
                    "/")))
        (find-file todo)
        (jdh-org-ensure-olp path)
        (forward-line -1)
        ))

    (setq org-capture-templates
          (list
           '("p" "Project TODO" entry
                 (function jdh--find-todo-location)
                 "* TODO %^{name|%i}
[[file:%(jdh--todo-relative-path \"%F\")::%i][source]]"
                 ))))

;; **** Outorg
  (with-eval-after-load 'outorg
    (defun jdh--outorg-wrap-source-in-block (fun lang &optional _)
      (funcall fun lang))

    (defun jdh--outorg-in-babel-load-languages-p (fun _) t)

    (defun jdh--outorg-copy-edits-and-exit ()
      (interactive)
      (outorg-copy-edits-and-exit)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp (format "^%s[[:space:]]*?\n%s [*]+ "
                                              comment-start
                                              comment-start)
                                      nil
                                      'noerror)
          (forward-line -1)
          (delete-region (point) (point-at-eol)))))

    (advice-add 'outorg-wrap-source-in-block :around 'jdh--outorg-wrap-source-in-block)

    (advice-add 'outorg-in-babel-load-languages-p :around 'jdh--outorg-in-babel-load-languages-p)

    (add-to-list 'outorg-language-name-assocs '(js-mode . js))
    (add-to-list 'outorg-language-name-assocs '(js2-mode . js))

    (spacemacs/set-leader-keys-for-minor-mode 'outorg-edit-minor-mode
      "ooe" 'jdh--outorg-copy-edits-and-exit)
    )

;; **** Org babel
  (with-eval-after-load 'ob
    (load-file "~/.emacs.d/ob-typescript.el")
    (require 'ob-typescript)
    (load-file "~/.emacs.d/ob-flowtype.el")
    (require 'ob-flowtype))

;; **** Org tree slide
  (with-eval-after-load 'org-tree-slide
    (spacemacs/set-leader-keys-for-minor-mode 'org-tree-slide-mode
      "Z" 'org-tree-slide-content))

;; *** outshine
  (with-eval-after-load 'outshine
    (defvar jdh--outorg-in-edit-p nil)

    (defun jdh--outorg-edit-as-org ()
      (interactive)
      (let ((jdh--outorg-in-edit-p t))
        (outorg-edit-as-org)))

    (defun jdh--outorg-edit-all-as-org ()
      (interactive)
      (let ((jdh--outorg-in-edit-p t))
        (outorg-edit-as-org '(4))))

    (spacemacs/set-leader-keys-for-minor-mode 'outline-minor-mode
      "ooe" 'jdh--outorg-edit-as-org
      "ooE" 'jdh--outorg-edit-all-as-org)
    )

;; *** persp
  (with-eval-after-load 'persp-mode
    (defun jdh-persp-remove-killed-buffers ()
      (interactive)
      (mapc #'(lambda (p)
                (when p
                  (setf (persp-buffers p)
                        (delete-if-not #'buffer-live-p (persp-buffers p)))))
            (persp-persps))))

;; *** purpose
  (with-eval-after-load 'window-purpose
    (purpose-compile-user-configuration))

;; *** spaceline
  (with-eval-after-load 'spaceline-segments
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-buffer-position-off))

;; ** Hacks
  (spacemacs/set-leader-keys
    dotspacemacs-emacs-command-key 'helm-M-x)
  )

;; * Custom
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-javascript-flow-args (quote ("--respect-pragma")))
 '(package-selected-packages
   (quote
    (toc-org org-bullets evil-nerd-commenter yaml-mode xterm-color ws-butler wolfram-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package typo tide thrift tagedit stan-mode sql-indent spacemacs-theme spaceline solarized-theme smeargle slim-mode slack shell-pop scss-mode scad-mode sass-mode restclient-helm restart-emacs rainbow-delimiters quelpa qml-mode pug-mode psci psc-ide powershell popwin persp-mode pcre2el pass paradox outshine orgtbl-ascii-plot orgit org-tree-slide org-projectile org-present org-pomodoro org-plus-contrib org-gcal org-download open-junk-file ob-restclient ob-http noflet nodejs-repl nix-mode neotree multi-term move-text mocha mmm-mode matlab-mode material-theme markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode kv julia-mode json-mode js2-refactor js-doc intero insert-shebang info+ indent-guide ido-vertical-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-purpose helm-projectile helm-nixos-options helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-chrome helm-c-yasnippet helm-aws helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter gh-md fsharp-mode flycheck-pos-tip flycheck-haskell flycheck-flow flx-ido fit-frame fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell excorporate evil-visualstar evil-visual-mark-mode evil-vimish-fold evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help ensime emmet-mode elisp-slime-nav dumb-jump dizzee dired-narrow define-word csharp-mode company-web company-tern company-statistics company-shell company-restclient company-nixos-options company-ghci company-ghc company-flow company-cabal column-enforce-mode coffee-mode cmm-mode clean-aindent-mode calfw bnfc auto-yasnippet auto-highlight-symbol auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((projectile-project-test-cmd . "yarn run build-then-test")
     (projectile-project-compilation-cmd . "yarn run build")
     (projectile-project-name . "MJS")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
