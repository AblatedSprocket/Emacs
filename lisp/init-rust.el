;;; init-rust.el --- Emacs configuration for Rust Mode
;;; Commentary:
;;; Mode included for Racer compatibility.
;;; Code:
(require 'init-elpa)
(require-package 'company)
(require-package 'rust-mode)
(require-package 'flycheck)
(require-package 'flycheck-rust)

(require 'company)
(require 'rust-mode)
(require 'flycheck)
(require 'flycheck-rust)
(require 'lsp-mode)

(setq lsp-rust-server 'rust-analyzer)

(setq lsp-rust-analyzer-server-command '("~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))

(setenv "PATH" (concat "/home/andy/.cargo/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/home/andy/.cargo/bin")

(defun cargo-build (arg)
  "Build with input ARG."
  (interactive "MCargo Build arguments: ")
  (compile (concat "cargo build " arg)))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'rust-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'flycheck-rust-setup)

(provide 'init-rust)
;;; init-rust.el ends here
