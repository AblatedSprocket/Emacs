;;; init-python.el --- Emacs configuration for Python
;;; Commentary:
;;; - Enable virtual environment support in Emacs
;;; Code:
(require-package 'auto-virtualenvwrapper)

(require 'auto-virtualenvwrapper)
(require 'init-code)
(require 'init-elpa)

;; Variables
(setenv "PATH" (concat "/home/andy/.local/bin:" (getenv "PATH")))

;; Hooks
;; (add-hook 'python-mode-hook
          ;; (lambda ()
            ;; (local-unset-key (kbd "C-c C-f"))))
(add-hook 'python-mode-hook 'autovirtualenvwrapper-activate)
(add-hook 'python-mode-hook 'init-code)
(add-hook 'python-mode-hook 'lsp)


(provide 'init-python)
;;; init-python.el ends here
