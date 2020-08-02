;;; init-writing.el --- Emacs configuration for general writing
;;; Commentary:
;;; - Enable thesaurus
;;; Code:
(require 'init-elpa)

(require-package 'markdown-mode)
(require-package 'olivetti)

(defun set-printing-font ()
  "Set font to Gentium."
  (face-remap-add-relative 'default '(:family "Gentium")))

(setq truncate-lines nil)

(add-hook 'markdown-mode-hook 'flyspell-mode)


(provide 'init-writing)
;;; init-writing.el ends here
