;;; init-code.el --- Emacs configuration for writing code.
;;; Commentary:
;;; - Forces spaces instead of tabs
;;; Code:
(require 'init-elpa)

(require-package 'company-quickhelp)
(require-package 'hl-todo)
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'lsp-treemacs)
(require-package 'magit)
(require-package 'rainbow-delimiters)
(require-package 'treemacs-magit)

(require 'hl-todo)
(require 'init-ui)
(require 'lsp-mode)

;; Functions

(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))

(defun set-indentation ()
  "Set indentation style."
  (setq indent-tabs-mode nil))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; Variables
(setq company-idle-delay 0
      company-quickhelp-delay 0
      company-quickhelp-color-background "#cfd8dc"
      company-quickhelp-color-foreground "#607d8b"
      company-selection-wrap-around nil
      company-tooltip-align-annotations t
      electric-pair-mode 1
      ;; lsp-signature-auto-activate t
      lsp-signature-doc-lines 1
      lsp-ui-doc-delay 0
      lsp-ui-doc-enable nil
      lsp-ui-doc--inline-ov t
      lsp-ui-sideline-enable nil)

;; Evaluations
(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-scope :project))

;; Keybindings
(define-key lsp-mode-map (kbd "C-c a") 'lsp-execute-code-action)
(define-key lsp-mode-map (kbd "C-c d") 'lsp-describe-thing-at-point)
(define-key lsp-mode-map (kbd "C-c s") 'lsp-find-references)
(define-key lsp-mode-map (kbd "C-c e") 'lsp-rename)
(define-key lsp-mode-map (kbd "C-c S") 'lsp-treemacs-symbols)
(define-key prog-mode-map (kbd "C-c h") 'hs-toggle-hiding)
;; Hooks
(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'fic-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'set-indentation)
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

(provide 'init-code)
;;; init-code.el ends here
