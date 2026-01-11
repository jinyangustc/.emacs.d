;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)


;; window size and features
(setq-default window-resize-pixelwise t
	      frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; (menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'default-frame-alist no-border))

(when *is-a-mac*
  (use-package ns-auto-titlebar
    :diminish ns-auto-titlebar-mode
    :config
    (ns-auto-titlebar-mode)))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
