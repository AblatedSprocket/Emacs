;;; init-writing.el --- Emacs configuration for general writing
;;; Commentary:
;;; - Enable thesaurus
;;; Code:
(require-package 'auctex)
(require-package 'company-auctex)
(require-package 'markdown-mode)
(require-package 'olivetti)

(require 'init-elpa)
(require 'init-ui)
(require 'company-auctex)

(latex-preview-pane-enable)

(defun set-printing-font ()
  "Set font to Gentium."
  (face-remap-add-relative 'default '(:family "Gentium")))

(setq truncate-lines nil)

;; TODO: Verify that this is unnecessary.
;; (add-hook 'markdown-mode-hook 'flyspell-mode)

(provide 'init-writing)
;;; init-writing.el ends here
