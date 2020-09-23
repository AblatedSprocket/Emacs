;;; init-python.el --- Emacs configuration for Python
;;; Commentary:
;;; - Enable virtual environment support in Emacs
;;; Code:
(require-package 'auto-virtualenvwrapper)

(require 'auto-virtualenvwrapper)
(require 'init-code)
(require 'init-elpa)
(require 'lsp-pyls)

;; Variables
(setenv "PATH" (concat "/home/andy/.local/bin:" (getenv "PATH")))

(add-to-list 'exec-path "/home/andy/.local/bin")

(setq lsp-pyls-plugins-pycodestyle-ignore '("E501"))
;; Hooks
;; (add-hook 'python-mode-hook
          ;; (lambda ()
;; (local-unset-key (kbd "C-c C-f"))))

(define-key python-mode-map (kbd "C-c r")
  (lambda()
    (interactive)
    (compile (concat "venv/bin/python3 " (buffer-name)))))
           
(add-hook 'python-mode-hook 'autovirtualenvwrapper-activate)
(add-hook 'python-mode-hook 'lsp)

(provide 'init-python)
;;; init-python.el ends here
