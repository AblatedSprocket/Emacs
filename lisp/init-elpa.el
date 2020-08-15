;;; init-elpa.el --- Creates require-package function to automate package installation
;;; Commentary:
;;; - Defines require-package function
;;; - Adds melpa archive

;;; Code:
(require 'package)

(defun require-package (package)
  "Install PACKAGE if it was not installed before."
  (if (package-installed-p package)
      t
    (progn
      (unless (assoc package package-archive-contents)
	(package-refresh-contents))
      (package-install package))))

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
;; (package-initialize)

(provide 'init-elpa)
;;; init-elpa.el ends here
