;;; init-bindings.el --- Initializes key bindings.
;;; Commentary:
;;; - All keybindings belong in this file for easy tracking and conflict resolution.
;;; Code:

(require 'init-code)
(require 'init-navigation)
(require 'init-rust)
(require 'init-ui)

;; Vanilla keybindings for any mode
(global-set-key (kbd "C-c C-<up>") 'toggle-frame-maximized)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)
(global-set-key (kbd "M-B") 'backward-to-word)
(global-set-key (kbd "C-c C-w") 'whack-whitespace)
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-c C-v") 'neotree-toggle)
(global-set-key (kbd "C-'") 'toggle-comment-on-line)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

;; Flyspell keybindings


;; LSP keybindings
(define-key lsp-mode-map (kbd "C-c C-e") 'lsp-describe-thing-at-point)
(define-key lsp-mode-map (kbd "C-c C-a") 'lsp-find-references)
(define-key lsp-mode-map (kbd "C-c C-r") 'lsp-rename)

;; Mu4e keybindings
(add-hook 'mu4e-view-mode-hook
          (lambda()
            (local-set-key (kbd "<RET>") 'mu4e-view-browse-url-from-binding)
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; Multiple cursors keybindings
(global-set-key (kbd "C-c C-m") 'mc/edit-lines)

;; Org keybindings
(global-set-key (kbd "C-c C-o") 'org-capture)

;; Projectile keybindings


;; Python keybindings
(add-hook 'python-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c C-f"))))
                             
;; Rust keybindings
(define-key rust-mode-map (kbd "C-c C-c c")
  (lambda ()
    (interactive)
    (compile "cargo check")))
(define-key rust-mode-map (kbd "C-c C-c b") 'cargo-build)
(define-key rust-mode-map (kbd "C-c C-c k")
  (lambda ()
    (interactive)
    (compile "cargo check")))
(define-key rust-mode-map (kbd "C-c C-c t")
  (lambda ()
    (interactive)
    (compile "cargo test")))

(provide 'init-bindings)
;;; init-bindings.el ends here
