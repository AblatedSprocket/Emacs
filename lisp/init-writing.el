;;; init-writing.el --- Emacs configuration for general writing
;;; Commentary:
;;; - Enable thesaurus
;;; Code:
(require 'init-elpa)

(require-package 'markdown-mode)

(defun set-printing-font ()
  "Set font to Gentium."
  (face-remap-add-relative 'default '(:family "Gentium")))

(setq truncate-lines nil)

(add-hook 'markdown-mode-hook 'flyspell-mode)

(add-hook 'org-mode-hook 'set-printing-font)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'toggle-truncate-lines)

(provide 'init-writing)
;;; init-writing.el ends here
