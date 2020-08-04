;;; init-code.el --- Emacs configuration for writing code.
;;; Commentary:
;;; - Forces spaces instead of tabs
;;; Code:
(require 'init-elpa)

(require-package 'company-lsp)
(require-package 'fic-mode)
(require-package 'lsp-mode)
;; (require-package 'lsp-ui)
(require-package 'lsp-treemacs)
(require-package 'magit)
(require-package 'rainbow-delimiters)
(require-package 'treemacs-magit)

(require 'company-lsp)
(require 'fic-mode)
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

(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "diagnostics")
        (server :default "localhost")        
        (port :default 5432)))
(setq company-idle-delay 0)
(setq company-selection-wrap-around nil)
(setq company-tooltip-align-annotations t)
(setq electric-pair-mode 1)
(setq truncate-lines nil)
;; (setq lsp-signature-auto-activate t)
(setq lsp-signature-doc-lines 1)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'set-indentation)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'fic-mode)
(add-hook 'elisp-mode-hook 'fic-mode)
(provide 'init-code)
;;; init-code.el ends here
