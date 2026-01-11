;;; init-isearch.el --- isearch settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package anzu
  :hook (after-init . global-anzu-mode)
  :config
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

(with-eval-after-load 'isearch
  ;; DEL during isearch should eidt the search string, not jump back to the previous result
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

  (defun sanityinc/isearch-occur ()
    "Invoke `consult-line' from isearch."
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (consult-line query)))

  (define-key isearch-mode-map (kbd "C-o") 'sanityinc/isearch-occur)
  (define-key isearch-mode-map (kbd "C-c C-o") 'sanityinc/isearch-occur))


(provide 'init-isearch)
;;; init-isearch.el ends here
