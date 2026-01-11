;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doric-themes
  :demand t
  :config
  (doric-themes-select 'doric-fire)
  ;; font
  (set-face-attribute 'default nil :family "Aporetic Serif Mono" :height 180)
  (set-face-attribute 'variable-pitch nil :family "Aporetic Serif" :height 1.0)
  (set-face-attribute 'fixed-pitch nil :family "Aporetic Serif Mono" :height 1.0))

;; (use-package modus-themes
;;   :demand t
;;   :init
;;   (modus-themes-include-derivatives-mode 1)
;;   :config
;;   (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
;;         modus-themes-to-rotate modus-themes-items
;;         modus-themes-mixed-fonts t
;;         modus-themes-variable-pitch-ui t
;;         modus-themes-italic-constructs t
;;         modus-themes-bold-constructs t
;;         modus-themes-completions '((t . (bold)))
;;         modus-themes-prompts '(bold)
;;         modus-themes-headings
;;         '((agenda-structure . (variable-pitch light 2.2))
;;           (agenda-date . (variable-pitch regular 1.3))
;;           (t . (regular 1.15))))
;;   (setq modus-themes-common-palette-overrides nil)
;;   (modus-themes-load-theme 'modus-vivendi-tinted))

(provide 'init-themes)
;;; init-themes.el ends here
