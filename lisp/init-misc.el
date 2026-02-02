;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'conf-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'sanityinc/set-mode-for-new-scripts)

(defun sanityinc/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))

(use-package info-colors
  :after info
  :hook (Info-selection . info-colors-fontify-node))

(use-package regex-tool
  :config
  (setq-default regex-tool-backend 'perl))

(with-eval-after-load 're-builder
  (setq reb-re-syntax 'string)
  ;; Support a slightly more idiomatic quit binding in re-builder
  (define-key reb-mode-map (kbd "C-c C-k") 'reb-quit))

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))


;; Bridging Re-Builder And Query-Replace-Regexp
;; See:
;; https://lmno.lol/alvaro/hey-mouse-dont-mess-with-my-emacs-font-size
;;
;; if you want to insert a newline in the regexp-builder buffer you
;; can now use C-q C-j

(defvar my/re-builder-positions nil
      "Store point and region bounds before calling re-builder")
    (advice-add 're-builder
                :before
                (defun my/re-builder-save-state (&rest _)
                  "Save into `my/re-builder-positions' the point and region
  positions before calling `re-builder'."
                            (setq my/re-builder-positions
                                  (cons (point)
                                        (when (region-active-p)
                                          (list (region-beginning)
                                                (region-end)))))))
  (defun reb-replace-regexp (&optional delimited)
    "Run `query-replace-regexp' with the contents of re-builder. With
  non-nil optional argument DELIMITED, only replace matches
  surrounded by word boundaries."
    (interactive "P")
    (reb-update-regexp)
    (let* ((re (reb-target-value 'reb-regexp))
           (replacement (query-replace-read-to
                         re
                         (concat "Query replace"
                                 (if current-prefix-arg
                                     (if (eq current-prefix-arg '-) " backward" " word")
                                   "")
                                 " regexp"
                                 (if (with-selected-window reb-target-window
                                       (region-active-p)) " in region" ""))
                         t))
           (pnt (car my/re-builder-positions))
           (beg (cadr my/re-builder-positions))
           (end (caddr my/re-builder-positions)))
      (with-selected-window reb-target-window
        (goto-char pnt) ; replace with (goto-char (match-beginning 0)) if you want
                        ; to control where in the buffer the replacement starts
                        ; with re-builder
        (setq my/re-builder-positions nil)
        (reb-quit)
        (query-replace-regexp re replacement delimited beg end))))

(with-eval-after-load 're-builder
  (define-key reb-mode-map (kbd "RET") #'reb-replace-regexp)
  (define-key reb-lisp-mode-map (kbd "RET") #'reb-replace-regexp))

(global-set-key (kbd "C-M-%") #'re-builder)

(provide 'init-misc)
;;; init-misc.el ends here
