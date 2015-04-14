;;; packages.el --- RevealJs Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Jeremy Hughes
;;
;; Author: Jeremy Hughes <jedahu@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar revealjs-pre-extensions
  '(
    revealjs-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun revealjs/init-revealjs-mode ()
  (use-package revealjs-mode
    :defer t
    :commands (revealjs-mode)
    :config
    (progn
      (spacemacs|define-micro-state revealjs-scroll
        :doc "[,] prev [.] next [<] prev h [>] next h"
        :bindings
        ("," revealjs-prev-slide)
        ("." revealjs-next-slide)
        ("<" revealjs-prev-horizontal-slide)
        (">" revealjs-next-horizontal-slide))
      (let ((mode-map (cdr (assoc 'revealjs-mode evil-leader--mode-maps))))
        (unless mode-map
          (push (cons 'revealjs-mode
                      (cdr (assoc 'markdown-mode evil-leader--mode-maps)))
                evil-leader--mode-maps)))
      (evil-leader/set-key-for-mode 'revealjs-mode
        "mn" 'spacemacs/revealjs-scroll-micro-state
        "mrh" 'revealjs-insert-horizontal-slide
        "mrv" 'revealjs-insert-vertical-slide))))
