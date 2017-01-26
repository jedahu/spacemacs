;; -*- mode: dotspacemacs -*-
;;; Pre init
(setq os-mswin? (member system-type '(windows-nt ms-dos cygwin)))

;;; dotspacemacs/layers
;;;; Layers
(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-configuration-layer-path nil
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
   dotspacemacs-delete-orphan-packages t

;;;; Exclusions
   dotspacemacs-excluded-packages
   '(haskell-yas
     org-bullets
     smartparens
     which-function-mode
     toc-org)
   ))

;;; dotspacemacs/init
;;;; Init
(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-startup-banner nil
   dotspacemacs-themes '(spacemacs-dark
                         solarized-dark
                         solarized-light
                         material)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 8.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
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
   dotspacemacs-persistent-server nil
   dotspacemacs-default-package-repository nil

;;;; Packages
   dotspacemacs-additional-packages
   '(
     bnfc
     company-flow
     csharp-mode
     dired-narrow
     evil-vimish-fold
     flycheck-flow
     git-gutter
     helm-aws
     helm-chrome
     kv
     material-theme
     mocha
     nodejs-repl
     ob-http
     org-tree-slide
     outshine
     purescript-mode
     yaml-mode
     ))
  (setq
   ))

;;; dotspacemacs/user-init
(defun dotspacemacs/user-init ()
  (setq outline-minor-mode-prefix "\M-*")
  )

;;; dotspacemacs/user-config
;;;; Doc
(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

;;;; Require
  (require 'generic-x)


;;;; Setq
;;;;; Global
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
  (setq explicit-bash-args '("--noediting" "--rcfile" "~/.bashrc_emacs" "-i"))
  (setq flow-executable "flow")
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
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
  (setq org-babel-default-header-args:sh
        '((:prologue . "exec 2>&1")
             (:epilogue . ":")
             (:results . "output verbatim")
             (:wrap . "ANSI")))
  (setq org-bullets-mode nil)
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
          (view . "overview")
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
  (setq org-pomodoro-format ":%s")
  (setq org-pomodoro-time-format "%.2m")
  (setq org-publish-use-timestamps-flag nil)
  (setq org-src-fontify-natively t)
  (setq org-tags-column -80)
  (setq powerline-default-separator 'utf-8)
  (setq powerline-utf-8-separator-left 124)
  (setq powerline-utf-8-separator-right 124)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'projectile-run-eshell)
  (setq shell-default-shell 'shell)
  (setq shr-external-browser 'browse-url-xdg-open)
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (setq spacemacs-theme-org-height nil)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq tab-width 4)
  (setq user-mail-address "jedahu@gmail.com")
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;;;;; Conditional
  (when os-mswin?
    (setq org-pomodoro-audio-player "sounder")
    (setq tramp-default-method "plink")
    )

;;;; Global modes
  (evil-vimish-fold-mode 1)
  (yas-global-mode 1)

;;;; File types
  (add-to-list 'auto-mode-alist '("\\.es\\.flow\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.es\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\.flow\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("/\\.babelrc\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.flowconfig\\'" . ini-generic-mode))

;;;; Functions
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

;;;; Modes
  (define-derived-mode ansi-mode fundamental-mode "ansi"
    "Fundamental mode that understands ANSI colors."
    (require 'ansi-color)
    (font-lock-add-keywords nil '((jdh--fontify-ansi-colors))))

;;;; Lists
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

;;;;; Hooks
  (add-hook 'focus-out-hook 'save-all)
  ;; (add-hook 'evil-normal-state-entry-hook 'save-buffer)
  (add-hook 'before-save-hook 'time-stamp)

;;;; Bindings
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

  (evil-define-key 'normal sh-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer)

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer)

  (evil-define-key 'normal js-mode-map
    (kbd "<backtab>") 'outshine-cycle-buffer)

  (evil-define-minor-mode-key 'normal 'outline-minor-mode
    (kbd "<backtab>") 'outshine-cycle-buffer)

  (spacemacs/set-leader-keys
    "p'" 'projectile-run-eshell)

;;;; Modules
;;;;; ssh
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

;;;;; C
  (with-eval-after-load 'c-mode
    (defun c-mode-common-setup ()
      (setq tab-width 4)
      (setq c-basic-offset tab-width)
      (c-set-offset 'arglist-intro '++)
      (c-set-offset 'substatement-open 0))
    (add-hook 'c-mode-common-hook 'c-mode-common-setup))

;;;;; ediff
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

;;;;; EPA
  (use-package epa-file
    :defer t
    :init (epa-file-enable)
    :config
    (progn
      (setq epa-file-select-keys t)))

;;;;; eshell
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

;;;;; flow
  (with-eval-after-load 'js2-mode
    (defun jdh-flow-type-at-pos ()
      "show type"
      (interactive)
      (let ((file (buffer-file-name))
            (line (line-number-at-pos))
            (col (current-column))
            (tmpf (make-temp-file "code"))
            (code (buffer-string)))
        (with-temp-file tmpf (insert code))
        (with-temp-buffer
          (shell-command (format "%s start" flow-executable))
          (shell-command
           (format "%s type-at-pos --from emacs %d %d < %s" flow-executable line (1+ col) tmpf)
           (current-buffer))
          (goto-char (point-min))
          (end-of-line)
          (message (buffer-substring-no-properties (point-min) (point))))))

    (defun jdh-flow-stop ()
      (interactive)
      (shell-command (format "%s stop" flow-executable)))

    (defun jdh-flow-check ()
      (interactive)
      (shell-command (format "%s check" flow-executable)))

    (spacemacs/set-leader-keys-for-major-mode 'js-mode
      "fc" 'jdh-flow-check
      "fs" 'jdh-flow-stop
      "ft" 'jdh-flow-type-at-pos)

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "fc" 'jdh-flow-check
      "fs" 'jdh-flow-stop
      "ft" 'jdh-flow-type-at-pos))

;;;;; flycheck
  (with-eval-after-load 'flycheck
    (require 'flycheck-flow)
    (add-hook 'js-mode-hook 'flycheck-mode))

;;;;; Helm
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

;;;;; ispell
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

;;;;; javascript
  (with-eval-after-load 'js
    (defun jdh--js-mode-setup ()
      (spacemacs/toggle-auto-completion-on)
      (outline-minor-mode 1)
      (save-excursion
        (goto-char 0)
        (when (ignore-errors (re-search-forward "^// *@flow\\>"))
          (setq company-backends '(company-yasnippet company-flow)))))

    (defun jdh--js2-mode-setup ()
      (js2-mode-hide-warnings-and-errors))

    (evil-define-key 'insert js-mode-map
      "\t" 'company-indent-or-complete-common)

    (evil-define-key 'insert js2-mode-map
      "\t" 'company-indent-or-complete-common)

    (add-hook 'js-mode-hook 'jdh--js-mode-setup t)
    (add-hook 'js2-mode-hook 'jdh--js-mode-setup t)
    (add-hook 'js2-mode-hook 'jdh--js2-mode-setup t)

;;;;;; overrides
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

;;;;; Markdown
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

;;;;; Misc
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(flow "^\\([^:\n]+\\):\\([0-9]+\\)$" 1 2))
    (setq compilation-error-regexp-alist '(flow))
    ;; (pushnew '("^at \\(.*?\\) line \\([0-9]+\\), column \\([0-9]+\\)" 1 2 3)
    ;;          compilation-error-regexp-alist)
    )

  (with-eval-after-load 'lisp-mode
    (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))

  (with-eval-after-load 'js
    (add-hook 'js-mode-hook 'outline-minor-mode))

  (with-eval-after-load 'shell
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

  (with-eval-after-load 'sh-script
    (add-hook 'sh-mode-hook 'outline-minor-mode))

  (with-eval-after-load 'outline
    (require 'outshine)
    (diminish 'outline-minor-mode)
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

;;;;; nix-mode
  (with-eval-after-load 'nix-mode
    (add-hook 'nix-mode-hook 'hs-minor-mode))

;;;;; Org
  (with-eval-after-load 'org
    (require 'org-agenda)

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
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (sh . t)
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
                              t))

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
    )

;;;;;; Org babel
  (with-eval-after-load 'ob
    (load-file "~/.emacs.d/ob-typescript.el")
    (require 'ob-typescript)
    (load-file "~/.emacs.d/ob-flowtype.el")
    (require 'ob-flowtype))

;;;;;; Org tree slide
  (with-eval-after-load 'org-tree-slide
    (spacemacs/set-leader-keys-for-minor-mode 'org-tree-slide-mode
      "Z" 'org-tree-slide-content))

;;;;; outshine
  (use-package outshine
    :defer t
    :commands (outshine-cycle-buffer outshine-hook-function)
    )

;;;;; persp
  (with-eval-after-load 'persp-mode
    (defun jdh-persp-remove-killed-buffers ()
      (interactive)
      (mapc #'(lambda (p)
                (when p
                  (setf (persp-buffers p)
                        (delete-if-not #'buffer-live-p (persp-buffers p)))))
            (persp-persps))))

;;;;; spaceline
  (with-eval-after-load 'spaceline-segments
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-buffer-position-off))
  )

;;; Custom
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "c:/Users/Jeremy.hughes/.emacs.d/.cache/bookmarks")
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(helm-source-names-using-follow (quote ("evil-markers" "mark-ring")))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (insert-shebang hide-comnt mocha sql-indent pcache org helm-gtags ggtags evil-unimpaired uuidgen tide typescript-mode thrift pug-mode org-projectile org-download livid-mode skewer-mode simple-httpd link-hint intero hlint-refactor helm-hoogle git-link eyebrowse evil-visual-mark-mode evil-ediff eshell-z dumb-jump company-shell company-ghci column-enforce-mode undo-tree flycheck-flow company-flow dired-rainbow dired-narrow dired-hacks-utils org-tree-slide ob-typescript git-gutter helm-chrome bookmark+ typo emojify oauth2 websocket ht password-store outshine outorg alert log4e gntp ob-http noflet nix-mode material-theme kv json-snatcher parent-mode helm-nixos-options request helm-aws haml-mode pkg-info epl flx evil-vimish-fold vimish-fold iedit highlight ensime sbt-mode scala-mode dash-functional pos-tip company-nixos-options nixos-options ghc bnfc dash slack circe anzu popup tern web-completion-data git-commit spinner package-build tss yaxception nodejs-repl psci deferred psc-ide powerline f hydra markdown-mode multiple-cursors js2-mode projectile smartparens packed avy company-quickhelp haskell-mode yasnippet company gitignore-mode helm helm-core json-reformat csharp-mode auto-complete flycheck magit magit-popup with-editor async s bind-key bind-map evil vi-tilde-fringe persp-mode evil-nerd-commenter yaml-mode xterm-color ws-butler wolfram-mode window-numbering which-key web-mode web-beautify volatile-highlights use-package toc-org tagedit stan-mode spacemacs-theme spaceline solarized-theme smooth-scrolling smeargle slim-mode shut-up shm shell-pop scss-mode scad-mode sass-mode restclient restart-emacs rainbow-delimiters quelpa qml-mode purescript-mode powershell popwin pcre2el pass paradox page-break-lines orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file omnisharp neotree multi-term move-text mmm-mode matlab-mode markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme less-css-mode julia-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md fsharp-mode flycheck-purescript flycheck-pos-tip flycheck-haskell flx-ido fish-mode fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-commentary evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help emmet-mode elisp-slime-nav define-word company-web company-tern company-statistics company-ghc company-cabal coffee-mode cmm-mode clean-aindent-mode buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-compile arduino-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((org-babel-default-header-args:typescript
      (:cmdline . "--noImplicitAny --strictNullChecks --pretty")
      (:wrap . "ANSI"))
     (org-confirm-babel-evaluate))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 83 :width normal))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
