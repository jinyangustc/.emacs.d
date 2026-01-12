;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)


;; huge files
(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))
(use-package vlf)
(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

;;; Newline behaviour (see also electric-indent-mode, enabled above)
;; (defun sanityinc/newline-at-end-of-line ()
;;   "Move to end of line, enter a newline, and reindent."
;;   (interactive)
;;   (move-end-of-line 1)
;;   (newline-and-indent))

;; (global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

(use-package subword
  :diminish subword-mode)

(use-package display-line-numbers-mode
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)
         (yaml-mode . display-line-numbers-mode)
         (yaml-ts-mode . display-line-numbers-mode))
  :config
  (setq-default display-line-numbers-width 3))


(use-package display-fill-column-indicator
  :ensure nil
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?┊))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Zap *up* to char is a handy pair for zap-to-char
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(add-hook 'after-init-hook 'show-paren-mode)

(when (fboundp 'repeat-mode)
  (add-hook 'after-init-hook 'repeat-mode))

(use-package avy
  :bind (("C-;" . avy-goto-char-timer)))

(use-package multiple-cursors
  :bind (("C-<"     . mc/mark-previous-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-+"     . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(use-package move-dup
  :bind (([M-S-up]   . move-dup-move-lines-up)
         ([M-S-down] . move-dup-move-lines-down)))


(use-package whole-line-or-region
  :hook (after-init . whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode)

;; M-^ is inconvenient, so also bind M-j
;; (global-set-key (kbd "M-j") 'join-line)

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-idle-delay 0.5)
  :diminish which-key-mode)

;; https://github.com/bbatsov/crux
(use-package crux
  :bind
  ("C-c o" . crux-open-with)
  ;; ("C-k" . crux-smart-kill-line)        ; first kill to end of the line, then kill the whole line
  ("C-," . crux-duplicate-current-line-or-region) ; duplicate the current line or region
  ;; ("M-o" . crux-other-window-or-switch-buffer)      ; select other win, or switch to most recent buff if only 1 win
  ("M-S-<return>" . crux-smart-open-line-above)              ; insert an empty line above the current line and indent it
  ("S-<return>" . crux-smart-open-line)                  ; insert an empty line and indent it
  ("M-j" . crux-top-join-line) ; join lines
  ("C-x C-u" . crux-upcase-region)
  ("C-x C-l" . crux-downcase-region)
  ("C-x M-c" . crux-capitalize-region))


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
