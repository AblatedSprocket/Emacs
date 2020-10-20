;;; init-rust.el --- Emacs configuration for Rust Mode
;;; Commentary:
;;; Mode included for Racer compatibility.
;;; Code:
(require 'init-elpa)
(require 'init-code)
(require-package 'rust-mode)
(require-package 'flycheck-rust)

(require 'company)
(require 'rust-mode)
(require 'flycheck)
(require 'flycheck-rust)
(require 'lsp-mode)

;; Functions
(defun cargo-build (arg)
  "Build with input ARG."
  (interactive "MCargo Build arguments: ")
  (compile (concat "cargo build " arg)))

;; Variables
(setenv "PATH" (concat "/home/andy/.cargo/bin:" (getenv "PATH")))

(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-analyzer-server-command '("~/.local/bin/rust-analyzer"))
(setq lsp-rust-server 'rust-analyzer)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'exec-path "/home/andy/.cargo/bin")

;; Keybindings
(define-key rust-mode-map "'" 'electric-pair)
(define-key rust-mode-map "\"" 'electric-pair)
(define-key rust-mode-map "(" 'electric-pair)
(define-key rust-mode-map "(" 'electric-pair)
(define-key rust-mode-map "[" 'electric-pair)
(define-key rust-mode-map "{" 'electric-pair)

(define-key rust-mode-map (kbd "C-c b") 'cargo-build)
(define-key rust-mode-map (kbd "C-c f") 'rust-format-buffer)
(define-key rust-mode-map (kbd "C-c r")
  (lambda ()
    (interactive)
    (compile "cargo run")))
(define-key rust-mode-map (kbd "C-c k")
  (lambda ()
    (interactive)
    (compile "cargo check")))
(define-key rust-mode-map (kbd "C-c t")
  (lambda ()
    (interactive)
    (compile "cargo test -- --nocapture")))
(define-key rust-mode-map (kbd "C-c C-f") nil)

;; Hooks
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'flycheck-rust-setup)

(provide 'init-rust)
;;; init-rust.el ends here
