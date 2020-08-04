;;; init-ui.el --- Initializes UI configuration for Emacs. General configuration for
;;; all modes is encapsulated by this file.
;;; Commentary:
;;; - Changes yes/no responses to y/n
;;; - Lock files are no longer generated
;;; - Installs ripgrep utilities (requires installation of ripgrep)


;;; Code:
(require 'init-elpa)

(require-package 'latex-preview-pane)
(require-package 'atom-one-dark-theme)
(require-package 'company)
(require-package 'flycheck)
(require-package 'material-theme)
(require-package 'rg)

(require 'company)
(require 'saveplace)

(require-package 'auctex)
(require-package 'company-auctex)
(require 'company-auctex)
(latex-preview-pane-enable)

;; Set color theme
;; (load-theme 'atom-one-dark t)
(load-theme 'material-light t)
;; Highlight matching parentheses
(show-paren-mode 1)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(blink-cursor-mode 1)
;; Highlight current line
(global-hl-line-mode 1)
(global-linum-mode 1)
(global-eldoc-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)
;; (company-auctex-init)
;; No need for ~ files when editing
(setq create-lockfiles nil)

(set-face-attribute 'default nil :font "Inconsolata" :height 140)
;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
(setq-default save-place t)
(setq-default word-wrap t)
(setq-default cursor-type 'bar)
;; (set-cursor-color "#coccyx")

;; Load .brick
(setq company-minimum-prefix-length 1)

(setq inhibit-startup-message t)

(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t)

;; Keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))
;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.config/emacs/backups.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(setq company-tooltip-align-annotations t)

;; Functions
(defun whack-whitespace (arg)
    "Deletes all white space from point to the next word. With prefix ARG delete across newlines as well. The only danger in this is that you don't have to actually be at the end of a word to make it work.  It skips over to the next whitespace and then whacks it all to the next word."
      (interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))

(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-visual-line-mode)
(provide 'init-ui)
;;; init-ui.el ends here
