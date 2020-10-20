;;; init-python.el --- Emacs configuration for Python
;;; Commentary:
;;; - Enable virtual environment support in Emacs
;;; Code:
(require-package 'blacken)
(require-package 'py-autopep8)
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
;; Hooks
(add-hook 'python-mode-hook (lambda ()
                             (local-set-key (kbd "C-c r")
                                            (lambda()
                                              (interactive)
                                              (compile (concat "venv/bin/python3 " (buffer-name)))))))
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'python-mode-hook 'lsp)
(add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'python-mode-hook
          (lambda()
            (local-set-key (kbd "C-c f") 'blacken-buffer)))
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(provide 'init-python)
;;; init-python.el ends here
