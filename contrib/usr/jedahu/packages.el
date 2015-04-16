(defvar jedahu-packages
  '(
    ahg
    eshell
    eshell-autojump
    git
    helm
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

(defun jedahu-setup-eshell ()
  (defun jedahu-eshell-init ()
    (define-key eshell-mode-map
      [remap eshell-pcomplete] 'helm-esh-pcomplete))

  (add-hook 'eshell-mode-hook 'jedahu-eshell-init)

  (defun jedahu-eshell-prompt ()
    (let ((header-bg "#fff"))
      (macrolet ((with-face (str &rest properties)
                            `(propertize ,str 'face ',properties)))
        (concat
         (with-face (concat (eshell/pwd) " ") :background header-bg)
         (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time))
                    :background header-bg :foreground "#888")
         (with-face
          (or (ignore-errors
                (format "(%s)"
                        (vc-responsible-backend default-directory))) "")
          :background header-bg)
         (with-face "\n" :background header-bg)
         (with-face user-login-name :foreground "blue")
         "@"
         (with-face "localhost" :foreground "green")
         (if (= (user-uid) 0)
             (with-face " #" :foreground "red")
           " $")
         " "))))

  (add-hook 'eshell-post-command-hook 'evil-normal-state)

  (setq
   eshell-prompt-function 'jedahu-eshell-prompt
   eshell-highlight-prompt nil))

(defun jedahu/init-eshell ()
  (eval-after-load 'eshell
    '(progn
       (jedahu-setup-eshell)
       (jedahu-setup-pcomplete-git)
       (jedahu-setup-pcomplete-hg))))

(defun jedahu/init-eshell-autojump ())

(defun jedahu/init-ahg ())
(defun jedahu/init-git ())
(defun jedahu/init-helm ())
(defun jedahu/init-rcirc ())
(defun jedahu/init-spinner ())
