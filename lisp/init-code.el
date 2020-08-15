;;; init-code.el --- Emacs configuration for writing code.
;;; Commentary:
;;; - Forces spaces instead of tabs
;;; Code:
(require 'init-elpa)

(require-package 'company-quickhelp)
(require-package 'company-lsp)
(require-package 'fic-mode)
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'lsp-treemacs)
(require-package 'magit)
(require-package 'rainbow-delimiters)
(require-package 'treemacs-magit)
(require-package 'yasnippet)

(require 'company-box)
(require 'company-lsp)
(require 'fic-mode)
(require 'init-ui)
(require 'lsp-mode)

;; Functions
(defun set-indentation ()
  "Set indentation style."
  (setq indent-tabs-mode nil))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; Variables
(setq company-idle-delay 0)
(setq company-quickhelp-delay 0)
(setq company-selection-wrap-around nil)
(setq company-tooltip-align-annotations t)
(setq electric-pair-mode 1)
;; (setq lsp-signature-auto-activate t)
(setq lsp-signature-doc-lines 1)
(setq lsp-ui-doc-delay 0)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc--inline-ov t)

;; Evaluations
(company-quickhelp-mode)
(yas-global-mode 1)
(push 'company-lsp company-backends)

;; Keybindings
(define-key lsp-mode-map (kbd "C-c a") 'lsp-find-references)
(define-key lsp-mode-map (kbd "C-c e") 'lsp-describe-thing-at-point)
(define-key lsp-mode-map (kbd "C-c r") 'lsp-rename)

;; Hooks
(add-hook 'elisp-mode-hook 'fic-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'fic-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'set-indentation)

(provide 'init-code)
;;; init-code.el ends here
