;;; init-markdown.el --- Markdown config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :mode ("\\.md\\.html\\'" . markdown-mode)
  :config
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

(provide 'init-markdown)
;;; init-markdown.el ends here
