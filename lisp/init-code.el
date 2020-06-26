;;; init-code.el --- Emacs configuration for writing code.
;;; Commentary:
;;; - Forces spaces instead of tabs
;;; Code:
(require 'init-elpa)

(require-package 'company-lsp)
(require-package 'lsp-mode)
;; (require-package 'lsp-ui)
(require-package 'lsp-treemacs)
(require-package 'magit)
(require-package 'rainbow-delimiters)
(require-package 'treemacs-magit)

(require 'company-lsp)
(require 'init-ui)
(require 'lsp-mode)

(push 'company-lsp company-backends)

(defun set-indentation ()
  "Set indentation style."
  (setq indent-tabs-mode nil))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(setq company-idle-delay 0)
(setq company-selection-wrap-around nil)
(setq company-tooltip-align-annotations t)
(setq truncate-lines nil)
;; (setq lsp-signature-auto-activate t)
(setq lsp-signature-doc-lines 1)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'set-indentation)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

(provide 'init-code)
;;; init-code.el ends here
