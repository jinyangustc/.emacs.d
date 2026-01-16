;;; init-compile.el --- Snippets configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; The official collection of snippets for yasnippet.
(use-package yasnippet-snippets
  :after yasnippet)

;; YASnippet is a template system designed that enhances text editing by
;; enabling users to define and use snippets. When a user types a short
;; abbreviation, YASnippet automatically expands it into a full template, which
;; can include placeholders, fields, and dynamic content.
(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-global-mode)
  :hook (after-init . yas-global-mode)
  :custom
  (yas-indent-line 'fixed) ; Do not auto-indent snippet content
  (yas-also-auto-indent-first-line nil)
  (yas-also-indent-empty-lines nil)
  (yas-snippet-revival nil)  ; Setting this to t causes issues with undo
  (yas-wrap-around-region nil) ; Do not wrap region when expanding snippets
  ;; (yas-triggers-in-field nil)  ; Disable nested snippet expansion
  ;; (yas-prompt-functions '(yas-no-prompt))  ; No prompt for snippet choices

  :init
  ;; Suppress verbose messages
  (setq yas-verbosity 0))

(use-package yasnippet-capf
  :after cape yasnippet)

(provide 'init-snippets)
;;; init-snippets.el ends here
