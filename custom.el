;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(epg-gpg-program
   (or
    (and (file-executable-p "/opt/homebrew/bin/gpg")
         "/opt/homebrew/bin/gpg")
    (and (file-executable-p "/usr/local/bin/gpg") "/usr/local/bin/gpg")
    "gpg"))
 '(package-selected-packages
   '(anzu avy cape command-log-mode consult consult-eglot corfu crux dash
          diff-hl diminish doric-themes exec-path-from-shell
          flymake-flycheck gcmh git-link git-modes git-timemachine
          gnu-elpa-keyring-update highlight-escape-sequences
          ibuffer-projectile ibuffer-vc info-colors just-mode
          just-ts-mode justl magit marginalia mode-line-bell
          modus-themes move-dup multiple-cursors ns-auto-titlebar
          orderless rainbow-delimiters regex-tool rg scratch shfmt
          sudo-edit vertico vlf whitespace-cleanup-mode
          whole-line-or-region)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
