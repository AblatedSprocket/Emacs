;;; init-ui.el --- Initializes UI configuration for Emacs. General configuration for
;;; all modes is encapsulated by this file.
;;; Commentary:
;;; - Saves list of 40 recently accessed files
;;; - General ido configuration
;;; - Auto-completion of M-x commands with ido
;;; - Enables project detection with Projectile
;;; - Disables ido use filename at point
;;; - Disables ido auto-merge work directories
;;; - Enables accessing closed buffers by enabling virtual buffers
;;; - Changes yes/no responses to y/n
;;; - Lock files are no longer generated
;;; - Installs ripgrep utilities (requires installation of ripgrep)
;;; Code:
(require-package 'company)
(require-package 'flycheck)
(require-package 'latex-preview-pane)
(require-package 'material-theme)
(require-package 'multiple-cursors)
(require-package 'projectile)
(require-package 'rg)
(require-package 'smex)
(require-package 'treemacs)
(require-package 'treemacs-projectile)

(require 'init-elpa)
(require 'company)
(require 'saveplace)
(require 'ido)
(require 'treemacs)
(require 'recentf)

;; Functions
(defun beginning-of-line-or-indentation ()
  "Move to beginning of line or indentation."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(defun switch-to-previous-buffer()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(defun whack-whitespace (arg)
    "Deletes all white space from point to the next word. With prefix ARG delete across newlines as well. The only danger in this is that you don't have to actually be at the end of a word to make it work.  It skips over to the next whitespace and then whacks it all to the next word."
      (interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))

;; Variables
(set-cursor-color "#000000")
(set-default 'truncate-lines t)
(set-face-attribute 'default nil :font "Inconsolata" :height 120)

(setq company-minimum-prefix-length 1)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)
(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t
 save-place-file (concat user-emacs-directory "places"))
(setq auto-save-default nil)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq ring-bell-function 'ignore)
(setq company-tooltip-align-annotations t)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(setq recentf-max-menu-items 40)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))

(setq-default save-place t)
(setq-default word-wrap t)
(setq-default cursor-type 'bar)

;; Evaluations
(blink-cursor-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode 1)
(global-eldoc-mode -1)
(ido-mode t)
(load-theme 'material-light t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(projectile-global-mode)
(recentf-mode 1)
(show-paren-mode 1)
(smex-initialize)
(windmove-default-keybindings)

;; Bindings
(global-set-key (kbd "C-'") 'toggle-comment-on-line)
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-c c") 'list-colors-display)
(global-set-key (kbd "C-c f") 'treemacs)
(global-set-key (kbd "C-c m") 'mc/edit-lines)
(global-set-key (kbd "C-c p") 'treemacs-display-current-project-exclusively)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-c C-<up>") 'toggle-frame-maximized)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-w") 'whack-whitespace)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-b") 'backward-word)
(global-set-key (kbd "M-B") 'backward-to-word)
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-x") 'smex)
(define-key treemacs-mode-map (kbd "C-d") 'treemacs-remove-project-from-workspace)
(define-key treemacs-mode-map (kbd "M-f") 'treemacs-next-project)
(define-key treemacs-mode-map (kbd "M-p") 'treemacs-previous-project)

;; Hooks
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-visual-line-mode)
(provide 'init-ui)
;;; init-ui.el ends here
