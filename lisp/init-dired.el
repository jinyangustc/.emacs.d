;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(use-package diff-hl
  :after dired
  :hook (dired-mode . diff-hl-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
