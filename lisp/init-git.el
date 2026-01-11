;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

(use-package git-modes)
(use-package git-timemachine
  :bind (("C-x v t" . git-timemachine-toggle)))
(use-package git-link)
(use-package magit
  :config
  (setq-default magit-diff-refine-hunk 'all)
  (setq-default magit-diff-visit-prefer-worktree t))

(provide 'init-git)
;;; init-git.el ends here
