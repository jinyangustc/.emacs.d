;;; init-gptel.el --- Configuration for gptel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :bind (("C-c RET" . gptel-send)
         ("C-c C-<return>" . gptel-menu)))

(straight-use-package
 '(gptel-prompts
   :type git
   :host github
   :repo "jwiegley/gptel-prompts"))

(use-package gptel-prompts
  :after (gptel)
  :demand t
  :config
  (gptel-prompts-update)
  ;; Ensure prompts are updated if prompt files change
  (gptel-prompts-add-update-watchers))

(provide 'init-gptel)

;;; init-gptel.el ends here
