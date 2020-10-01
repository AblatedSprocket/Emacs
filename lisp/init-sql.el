;;; init-sql.el --- Initializes SQL configuration
;;; Commentary:
;;; Create SQL buffer using sql-postgres
;;; Code:
(require-package 'sqlup-mode)
(require 'init-code)

;; Functions
(defun my-sql-disable-font-lock (orig-fun &rest args)
  "Disable syntax highlighting for SQL output."
  (cl-letf (((symbol-function #'sql-product-font-lock) #'ignore))
    (apply orig-fun args)))
  
(defun my-sql-login-hook ()
  "Custom SQL log-in behaviors."
  (when (eq sql-product 'postgres)
    (let ((proc (get-buffer-process (current-buffer))))
      (comint-send-string proc "\\set ECHO queries\n"))))

;; Variables
(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "smart_home")
        (server :default "localhost")
        (port :default 5432)))
(setq sql-product 'postgres)

(advice-add 'sql-interactive-mode :around 'my-sql-disable-font-lock)
;; Hooks
(add-hook 'sql-mode-hook 'sql-set-sqli-buffer)
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-mode-hook '(lambda ()
                            (setq truncate-lines t
                                  word-wrap nil)))
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
;; (add-hook 'sql-login-hook 'my-sql-login-hook)

(provide 'init-sql)
;;; init-sql.el ends here
