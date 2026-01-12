;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;;  https://github.com/purcell/emacs.d
;;;  https://github.com/jamescherti/minimal-emacs.d
;;;
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.
;;;
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;;; Bootstrap config
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (require 'init-utils)
;; (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el

(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;; general performance tuning
(use-package gcmh
  :diminish gcmh-mode
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024)))

(setq jit-lock-defer-time 0)

(use-package diminish)
(use-package scratch)
(use-package command-log-mode)

(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)
(require 'init-eglot)

(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-corfu)
(require 'init-windows)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-git)

(require 'init-projectile)

;; language modes
(require 'init-compile)
(require 'init-snippets)
(require 'init-python)
;; (use-package just-mode)
(use-package just-ts-mode)
(use-package justl)

(require 'init-spelling)
(require 'init-misc)

(use-package sudo-edit
  :bind ("C-c C-r" . sudo-edit))

(use-package shfmt)

(use-package eldoc
  :ensure nil
  :hook (after-init . global-eldoc-mode)
  :diminish eldoc-mode)

(require 'init-treesitter)


;; Emacs server
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))


(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-locales)

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
