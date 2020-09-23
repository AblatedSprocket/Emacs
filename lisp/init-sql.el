;;; init-sql.el --- Initializes SQL configuration
;;; Commentary:
;;; Create SQL buffer using sql-postgres
;;; Code:
(require-package 'sqlup-mode)
(require 'init-code)

;; Functions
(defun my-sql-login-hook ()
  "Custom SQL log-in behaviors."
  (when (eq-sql-product 'postgres)
    (let ((proc (get-buffer-process (current-buffer))))
      (comint-send-string proc "\\set ECHO queries\n"))))

;; Variables
(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "smart_home")
        (server :default "localhost")
        (port :default 5432)))

;; Hooks
(add-hook 'sql-login-hook 'my-sql-login-hook)

(provide 'init-sql)
;;; init-sql.el ends here
