;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

(use-package git-modes)
(use-package git-timemachine
  :bind (("C-x v t" . git-timemachine-toggle)))
(use-package git-link)
(use-package magit
  :bind ("<f5>" . magit-status)
  :config
  (setq-default magit-diff-refine-hunk 'all)
  (setq-default magit-diff-visit-prefer-worktree t))

;; (use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode))

(use-package casual
  :init
  (casual-ediff-install) ; run this to enable Casual Ediff
  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
            (keymap-set ediff-mode-map "C-o" #'casual-ediff-tmenu))))

(provide 'init-git)
;;; init-git.el ends here
