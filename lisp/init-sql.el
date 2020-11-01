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
(setq sql-connection-alist
      '(
        (home (sql-product 'postgres)
              (sql-port 5432)
              (sql-server "localhost")
              (sql-user "postgres")
              (sql-database "savetheglobe"))
        (savetheglobe_home (sql-product 'postgres)
                           (sql-port 5432)
                           (sql-server "localhost")
                           (sql-user "postgres")
                           (sql-database "savetheglobe"))
        (savetheglobe_heroku (sql-product 'postgres)
                             (sql-port 5432)
                             (sql-server "ec2-52-87-22-151.compute-1.amazonaws.com")
                             (sql-user "nrsgquqvfevzbu")
                             (sql-database "ddpfocn81le95m"))))

;; Evaluations
(with-eval-after-load "sql"
  (define-key sql-mode-map (kbd "C-c s") 'sql-send-region)
  (define-key sql-mode-map (kbd "C-c S") 'sql-send-buffer))
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
