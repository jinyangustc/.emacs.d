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

(require 'init-osx-keys)
(require 'init-themes)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-grep)
(require 'init-isearch)

(require 'init-recentf)
(require 'init-minibuffer)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-git)

(require 'init-projectile)

(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
