;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'package)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name
       (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
       user-emacs-directory))

;; Make sure signature keyring follows package-user-dir
(setq package-gnupghome-dir
      (expand-file-name "gnupg" package-user-dir))

;; (custom-set-variables
;;  '(epg-gpg-program "/opt/homebrew/bin/gpg"))

;; ;; macOS: point Emacs at Homebrew gpg explicitly
;; (let ((gpg (or (executable-find "gpg")
;;                (and (file-executable-p "/opt/homebrew/bin/gpg") "/opt/homebrew/bin/gpg")
;;                (and (file-executable-p "/usr/local/bin/gpg") "/usr/local/bin/gpg"))))
;;   (when gpg
;;     (custom-set-variables '(epg-gpg-program gpg))))

(when (eq system-type 'darwin)
  (custom-set-variables
   '(epg-gpg-program
     (or (and (file-executable-p "/opt/homebrew/bin/gpg") "/opt/homebrew/bin/gpg") ; Apple Silicon Homebrew
	 (and (file-executable-p "/usr/local/bin/gpg") "/usr/local/bin/gpg")	; Intel Homebrew
	 "gpg"))))

(make-directory package-user-dir t)
(make-directory package-gnupghome-dir t)

(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 70)
                                   ("melpa-stable" . 50)))
(add-to-list 'package-unsigned-archives "melpa")

(setq package-enable-at-startup nil)
(setq package-install-upgrade-built-in t)
(setq package-native-compile t)

;; Initialize and refresh package contents again if needed
(package-initialize)


(unless (package-installed-p 'gnu-elpa-keyring-update)
  (when (null package-archive-contents)
    (package-refresh-contents))
  (package-install 'gnu-elpa-keyring-update))

(require 'gnu-elpa-keyring-update)
(gnu-elpa-keyring-update)

;; Install use-package if necessary
(unless (package-installed-p 'use-package)
  (when (null package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

(provide 'init-elpa)
;;; init-elpa.el ends here
