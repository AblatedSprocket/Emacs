;;; init-dnd.el --- Configuration for org-d20
;;; Commentary:
;;; Code:
(require-package 'org-d20)

(require 'init-org)

;; Variables
(add-to-list 'org-capture-templates
             '("n" "NPC" entry (file+headline "~/org/dnd.org" "NPCs")
               "* %?\n %i\n %a"))

(setq org-capture-templates-contexts '(("n" ((in-file . "dnd.org")))))

;; Keybindings
(add-hook 'org-d20-mode-hook
         (lambda()
           (local-set-key (kbd "C-c a") 'org-d20-initiative-add)
           (local-set-key (kbd "C-c d") 'org-d20-damage)
           (local-set-key (kbd "C-c i") 'org-d20-initiative-dwim)
           (local-set-key (kbd "C-c r") 'org-d20-roll)
           (local-set-key (kbd "C-c R") 'org-d20-d20)))

(provide 'init-dnd)
;;; init-dnd.el ends here
