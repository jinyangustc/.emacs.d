;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
	      grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(use-package dash)

(use-package wgrep
  :after grep
  :bind (:map grep-mode-map
	      ("C-c C-q" . wgrep-change-to-wgrep-mode)
	      ("w" . wgrep-change-to-wgrep-mode)))


(use-package rg
  :if (executable-find "rg")
  :config
  (rg-enable-default-bindings))

(provide 'init-grep)
;;; init-grep.el ends here
