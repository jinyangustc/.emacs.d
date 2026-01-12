;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :diminish winner-mode)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g))
  (setq aw-background nil))

(provide 'init-windows)
;;; init-windows.el ends here
