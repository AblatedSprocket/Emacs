
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:

;; Uncomment this on new installation.
(package-initialize)

(package-refresh-contents)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))

(require 'init-elpa)
(require 'init-ui)
(require 'init-navigation)
(require 'init-writing)
(require 'init-code)
(require 'init-python)
(require 'init-rust)
(require 'init-mail)
(require 'init-bindings)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default)))
 '(package-selected-packages
   (quote
    (org-mime smtpmail-multi cargo-outdated pyvenv-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
