;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot)
(use-package consult-eglot)

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)

  :custom
  (xref-search-program 'ripgrep)

  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'init-eglot)
;;; init-eglot.el ends here
