;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot)
(use-package consult-eglot)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'rg)
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  )

(provide 'init-eglot)
;;; init-eglot.el ends here
