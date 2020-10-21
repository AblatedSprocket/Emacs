;; init-python.el --- Emacs configuration for Python
;;; Commentary:
;;; - Enable virtual environment support in Emacs
;;; Code:
(require-package 'blacken)
(require-package 'pyvenv)

(require 'blacken)
(require 'init-code)
(require 'init-elpa)
(require 'lsp-pyls)

;; Variables
(setenv "PATH" (concat "/home/andy/.local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/home/andy/.local/bin")

;; (setq lsp-pyls-plugins-pycodestyle-ignore '("E501", "W293", "E225"))
(setq lsp-pyls-plugins-pycodestyle-ignore '("E501")
      pyvenv-default-virtual-env-name "venv")

;; Bindings
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c r")
     (lambda()
       (interactive)
       (compile (concat "venv/bin/python3 " (buffer-name))))))
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c f") 'blacken-buffer))

;; Hooks
(add-hook 'python-mode-hook 'lsp)
(add-hook 'python-mode-hook 'pyvenv-mode)

(provide 'init-python)
;;; init-python.el ends here
