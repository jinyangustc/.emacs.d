;;; init-formatter.el --- Formatter -*- lexical-binding: t; -*-
;;; Commentary:
;;;   See: https://ddavis.io/blog/python-emacs-4/
;;; Code:

(use-package reformatter
  :config
  (reformatter-define dd/ruff-format
    :program "uvx"
    :args `("ruff" "format" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define dd/ruff-sort
    :program "uvx"
    :args `("ruff" "check" "--select" "I" "--fix" "--stdin-filename" ,buffer-file-name "-")))

(provide 'init-formatter)
;;; init-formatter.el ends here
