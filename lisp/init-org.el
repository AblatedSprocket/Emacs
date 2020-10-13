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
(setq org-agenda-breadcrumbs-separator " ❱ ")
(setq org-agenda-category-icon-alist '(("Appointment" "~/.config/emacs/icons/bell.svg" nil nil :ascent center)
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
                                       ("Todo" "~/.config/emacs/icons/gears.svg" nil nil :ascent center)))
(setq org-agenda-files '("~/org/tasks/Todo.org"))
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/tasks/Todo.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("d" "dream" entry (file "~/org/dreams/Dreams.org")
         "* %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%?")))
(setq org-directory "~/org")
(setq org-journal-date-format "%A, %B %d %Y")
(setq org-journal-dir "~/org/journal/")
(setq org-journal-enable-agenda-integration t)
(setq org-journal-file-format "%Y.org")
(setq org-journal-file-type "yearly")
(setq org-journal-skip-carryover-drawers t)
(setq org-log-into-drawer "LOGBOOK")
(setq org-modules '(org-habit))
(setq org-mu4e-convert-to-html t)
(setq org-mu4e-link-query-in-headers-mode nil)
(setq org-roam-capture--file-name-default "%<%Y%m%d>")
(setq org-roam-directory "~/org-roam")
(setq org-roam-capture-templates
      '(("r" "roam" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d>-${slug}"
         :head "#+title: ${title}\n"
         :unnarrowed t)))
(eval-after-load 'org
  '(org-load-modules-maybe t))

(add-to-list 'org-agenda-custom-commands
             '("x" "Testing tags for negating DONE" tags "-TODO=\"DONE\"" nil nil ))
(add-to-list 'org-agenda-files org-journal-dir)

;; Keybindings
(global-set-key (kbd "C-o") 'org-capture)
(global-set-key (kbd "C-c C-r c") 'org-roam-capture)
(global-set-key (kbd "C-c C-r f") 'org-roam-find-file)
(global-set-key (kbd "C-c C-r g") 'org-roam-graph)
(global-set-key (kbd "C-c C-r i") 'org-roam-insert)

;; Hooks
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'set-printing-font)

(provide 'init-org)
;;; init-org.el ends here
