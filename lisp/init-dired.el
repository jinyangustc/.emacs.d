;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)
(setq dired-use-ls-dired nil)
(setq dired-movement-style 'bounded-files)

;; dired: Group directories first
(with-eval-after-load 'dired
  (let ((args "--group-directories-first -ahlv"))
    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        (setq args nil)))
    (when args
      (setq dired-listing-switches args))))

(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(use-package casual
  :after dired
  :bind (:map dired-mode-map
              ("C-o" . #'casual-dired-tmenu)
              ("s" . #'casual-dired-sort-by-tmenu)
              ("/" . #'casual-dired-search-replace-tmenu)))

(provide 'init-dired)
;;; init-dired.el ends here
