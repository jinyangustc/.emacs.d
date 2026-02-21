;;; init-tramp.el --- Configuration for TRAMP -*- lexical-binding: t -*-
;;; Commentary:
;;; See: https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
;;; Code:

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)

(with-eval-after-load 'tramp
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

(with-eval-after-load 'magit
  (setq magit-tramp-pipe-stty-settings 'pty))

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook
     'compilation-mode-hook
     #'tramp-compile-disable-ssh-controlmaster-options)))


;; performance improvement for magit over TRAMP
;; (with-eval-after-load 'tramp
;;   (with-eval-after-load 'magit
;;     ;; don't show the diff by default in the commit buffer. Use `C-c
;;     ;; C-d' to display it
;;     (setq magit-commit-show-diff nil)
;;     ;; don't show git variables in magit branch
;;     (setq magit-branch-direct-configure nil)
;;     ;; don't automatically refresh the status buffer after running a
;;     ;; git command
;;     (setq magit-refresh-status-buffer nil)))

(provide 'init-tramp)

;;; init-tramp.el ends here
