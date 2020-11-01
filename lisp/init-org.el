;;; init-org.el --- Configuration for org-mode
;;; Commentary:
;;; - Manually install org-habit
;;; - Manually install org-mu4e
;;; Code:
(require-package 'org-journal)
(require-package 'org-mime)
(require-package 'org-roam)

(require 'init-writing)
(require 'init-mail)
(require 'org-habit)
(require 'org-journal)
(require 'org-mu4e)

;; Functions
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; Variables
(define-prefix-command 'ring-map)
(global-set-key (kbd "C-o") 'ring-map)
(setq org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-category-icon-alist '(("Appointment" "~/.config/emacs/icons/bell.svg" nil nil :ascent center)
                                       ("Cleaning" "~/.config/emacs/icons/flower.svg" nil nil :ascent center)
                                       ("Contractor" "~/.config/emacs/icons/tools.svg" nil nil :ascent center)
                                       ("Exercise" "~/.config/emacs/icons/barbell.svg" nil nil :ascent center)
                                       ("Finance" "~/.config/emacs/icons/columns.svg" nil nil :ascent center)
                                       ("Journal" "~/.config/emacs/icons/journal.svg" nil nil :ascent center)
                                       ("Learning" "~/.config/emacs/icons/flask.svg" nil nil :ascent center)
                                       ("Life" "~/.config/emacs/icons/leaf.svg" nil nil :ascent center)
                                       ("Maintenance" "~/.config/emacs/icons/wrench.svg" nil nil :ascent center)
                                       ("Organizing" "~/.config/emacs/icons/folder.svg" nil nil :ascent center)
                                       ("Party" "~/.config/emacs/icons/beer.svg" nil nil :ascent center)
                                       ("Todo" "~/.config/emacs/icons/gears.svg" nil nil :ascent center))
      org-agenda-files '("~/org/tasks/Todo.org")
      org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/tasks/Todo.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("d" "dream" entry (file "~/org/dreams/Dreams.org")
         "* %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%?"))
      org-directory "~/org"
      org-journal-date-format "%A, %B %d %Y"
      org-journal-dir "~/org/journal/"
      org-journal-enable-agenda-integration t
      org-journal-file-format "%Y.org"
      org-journal-file-type "yearly"
      org-journal-skip-carryover-drawers t
      org-log-into-drawer "LOGBOOK"
      org-modules '(org-habit)
      org-mu4e-convert-to-html t
      org-mu4e-link-query-in-headers-mode nil
      org-roam-capture--file-name-default "%<%Y%m%d>"
      org-roam-completion-system 'ido
      org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d>-${slug}"
         :head "#+title: ${title}\n"
         :unnarrowed t))
      org-roam-directory "~/org-roam")
(with-eval-after-load "org"
  (org-load-modules-maybe t)
  (define-key org-mode-map (kbd "C-c i") 'org-insert-link)
  (define-key org-mode-map (kbd "C-c f") 'org-roam-insert)
  (define-key org-mode-map (kbd "<M-return>") nil)
  (define-key org-mode-map (kbd "<C-return>") 'org-insert-heading))

(add-to-list 'org-agenda-custom-commands
             '("x" "Testing tags for negating DONE" tags "-TODO=\"DONE\"" nil nil ))
(add-to-list 'org-agenda-files org-journal-dir)

;; Keybindings
(global-set-key (kbd "C-o c") 'org-capture)
(global-set-key (kbd "C-o C-r c") 'org-roam-capture)
(global-set-key (kbd "C-o C-r f") 'org-roam-find-file)
(global-set-key (kbd "C-o C-r g") 'org-roam-graph)
(global-set-key (kbd "C-o C-r i") 'org-roam-insert)

;; Hooks
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'set-printing-font)

(provide 'init-org)
;;; init-org.el ends here
