;;; init-sql.el --- Initializes SQL configuration
;;; Commentary:
;;; None
;;; Code:
(require 'init-code)
(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "diagnostics")
        (server :default "localhost")        
        (port :default 5432)))
(provide 'init-sql)
;;; init-sql.el ends here
