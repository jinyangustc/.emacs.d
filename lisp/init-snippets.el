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
;;
;; For latex related config see:
;; https://gist.github.com/karthink/7d89df35ee9b7ac0c93d0177b862dadb
(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-global-mode)
  :hook ((after-init . yas-global-mode)
         (post-self-insert .  my/yas-try-expanding-auto-snippets))
  :bind (:map yas-keymap
              ("<tab>" . yas-next-field-or-cdlatex)
              ("TAB" . yas-next-field-or-cdlatex))
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
  (setq yas-verbosity 0)

  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-triggers-in-field t)

  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))


  (defun cdlatex-in-yas-field ()
    ;; Check if we're at the end of the Yas field
    (when-let* ((_ (overlayp yas--active-field-overlay))
                (end (overlay-end yas--active-field-overlay)))
      (if (>= (point) end)
          ;; Call yas-next-field if cdlatex can't expand here
          (let ((s (thing-at-point 'sexp)))
            (unless (and s (assoc (substring-no-properties s)
                                  cdlatex-command-alist-comb))
              (yas-next-field-or-maybe-expand)
              t))
        ;; otherwise expand and jump to the correct location
        (let (cdlatex-tab-hook minp)
          (setq minp
                (min (save-excursion (cdlatex-tab)
                                     (point))
                     (overlay-end yas--active-field-overlay)))
          (goto-char minp) t))))

  (defun yas-next-field-or-cdlatex nil
    (interactive)
    "Jump to the next Yas field correctly with cdlatex active."
    (if
        (or (bound-and-true-p cdlatex-mode)
            (bound-and-true-p org-cdlatex-mode))
        (cdlatex-tab)
      (yas-next-field-or-maybe-expand))))

(use-package yasnippet-capf
  :after cape yasnippet)

(provide 'init-snippets)
;;; init-snippets.el ends here
