;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package flymake
  :bind (:map flymake-mode-map
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! c" . flymake-start)))

(use-package flymake-flycheck
  :after flycheck
  :hook ((flymake-mode . flymake-flycheck-auto)
         (prog-mode . flymake-mode)
         (text-mode . flymake-mode))
  :config
  (with-eval-after-load 'flycheck
    (setq-default
     flycheck-disabled-checkers
     (append (default-value 'flycheck-disabled-checkers)
             '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck)))))

(provide 'init-flymake)
;;; init-flymake.el ends here
