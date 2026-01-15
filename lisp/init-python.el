;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

;; (setq python-shell-interpreter "python3")

;; (use-package pip-requirements)

(use-package flymake-ruff
  :hook (python-mode . sanityinc/flymake-ruff-maybe-enable)
  :config
  (defun sanityinc/flymake-ruff-maybe-enable ()
    (when (executable-find "ruff")
      (flymake-ruff-load))))

(with-eval-after-load 'eglot
  ;; This addition of "ty" has been upstreamed as of Dec 2025, but not
  ;; yet released in ELPA versions of eglot.
  (push `((python-mode python-ts-mode)
          . ,(eglot-alternatives
              '(("ty" "server")
                "pylsp"
                "pyls"
                ("basedpyright-langserver" "--stdio")
                ("pyright-langserver" "--stdio")
                ("pyrefly" "lsp")
                "jedi-language-server" ("ruff" "server") "ruff-lsp")))
        eglot-server-programs))

;; (use-package ruff-format)

(use-package toml-mode
  :mode ("\\(poetry\\|uv\\)\\.lock\\'" . toml-mode))

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml"))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files "pyproject.toml"))

(with-eval-after-load 'repeat
  (defvar my/python-indent-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<") #'python-indent-shift-left)
      (define-key map (kbd ">") #'python-indent-shift-right)
      map)
    "Repeat map for Python indent shifting.")

  (put 'python-indent-shift-left  'repeat-map 'my/python-indent-repeat-map)
  (put 'python-indent-shift-right 'repeat-map 'my/python-indent-repeat-map))


;; See: https://ddavis.io/blog/python-emacs-4/

(use-package pyvenv)

(defun dd/python-init ()
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (venv-path (when project-root
                      (expand-file-name ".venv" project-root))))
    (when (and venv-path (file-directory-p venv-path))
      (make-local-variable 'pyvenv-virtual-env)
      (pyvenv-activate venv-path))
    (dd/ruff-format-on-save-mode +1)
    (dd/ruff-sort-on-save-mode +1)))

(add-hook 'python-base-mode-hook #'dd/python-init)

(provide 'init-python)
;;; init-python.el ends here
