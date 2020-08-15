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

;; Functions
(defun set-printing-font ()
  "Set font to Gentium."
  (face-remap-add-relative 'default '(:family "Gentium")))

;; Variables
(setq olivetti-body-width 84)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . olivetti-mode))

;; Keybindings
(add-hook 'TeX-mode-hook
          (lambda()
            ;; (local-set-key (kbd "C-c r") 'latex-preview-pane-mode)))
            (local-set-key (kbd "C-c r") 'latex-preview-pane-mode)))

;; Hooks
(add-hook 'olivetti-mode-hook
         (lambda()
           (setq display-line-numbers nil)))
(add-hook 'olivetti-mode-hook 'set-printing-font)

(provide 'init-writing)
;;; init-writing.el ends here
