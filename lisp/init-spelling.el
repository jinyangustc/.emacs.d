;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ispell
  :commands (ispell ispell-minor-mode)
  :custom
  ;; Set the ispell program name to aspell
  (ispell-program-name "aspell")

  ;; Define the "en_US" spell-check dictionary locally, telling Emacs to use
  ;; UTF-8 encoding, match words using alphabetic characters, allow apostrophes
  ;; inside words, treat non-alphabetic characters as word boundaries, and pass
  ;; -d en_US to the underlying spell-check program.
  (ispell-local-dictionary-alist
   '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  ;; Configures Aspell's suggestion mode to "ultra", which provides more
  ;; aggressive and detailed suggestions for misspelled words. The language
  ;; is set to "en_US" for US English, which can be replaced with your desired
  ;; language code (e.g., "en_GB" for British English, "de_DE" for German).
  ;; --lang-en_US
  (ispell-extra-args '("--sug-mode=ultra")))


(use-package flyspell
  :commands flyspell-mode
  :bind (:map flyspell-mode-map
              ("C-;" . nil)
              ("C-," . nil)
              ("C-c w n" . flyspell-goto-next-error)
              ("C-c w p" . flyspell-goto-previous-error)
              ("C-c w c" . flyspell-correct-word-before-point)
              ("C-c w b" . flyspell-buffer)
              ("C-c w d" . flyspell-change-dictionary)
              ("C-c w w" . flyspell-word))
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . (lambda()
                  (if (or (derived-mode-p 'yaml-mode)
                          (derived-mode-p 'yaml-ts-mode)
                          (derived-mode-p 'ansible-mode))
                      (flyspell-prog-mode 1)
                    (flyspell-mode 1)))))
  :config
  ;; Remove strings from Flyspell
  (setq flyspell-prog-text-faces (delq 'font-lock-string-face
                                       flyspell-prog-text-faces))

  ;; Remove doc from Flyspell
  (setq flyspell-prog-text-faces (delq 'font-lock-doc-face
                                       flyspell-prog-text-faces)))

(provide 'init-spelling)
;;; init-spelling.el ends here
