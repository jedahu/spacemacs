(defvar irc-packages '(rcirc))

(defun irc/init-rcirc ()
  (use-package rcirc
    :defer t
    :config
    (progn
      (setq rcirc-server-alist
            '(("irc.freenode.net" :port 6697 :encryption tls
               :channels ("#nixos" "#purescript"))))
      (setq rcirc-authinfo-file (concat user-emacs-directory "rcirc-authinfo")))))
