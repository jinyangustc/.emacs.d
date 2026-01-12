;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :hook (after-init . projectile.mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq-default projectile-mode-line-prefix " Proj")
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0")))

(use-package ibuffer-projectile
  :after (projectile ibuffer))

(provide 'init-projectile)
;;; init-projectile.el ends here
