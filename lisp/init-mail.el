;;; init-mail.el --- Emacs configuration for gmail.
;;; Commentary:
;;; Refer to the README for dependencies and installing EMACS with support for mu4e.
;;; Code:
(require 'init-elpa)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)
;; Remove if mu4e-views doesn't pan out.
;; (add-to-list 'load-path "~/source/mu4e-views")
;; (require 'mu4e-views)
(require 'smtpmail)

;; Functions
(defun mu4e-show-in-browser ()
  "Show an email in the default web browser."
  (interactive)
  (mu4e-action-view-in-browser (mu4e-action-view-in-browser (mu4e-message-at-point t))))

(defun mu4e-view-in-browser-webkit (msg)
  "View the email MSG in embedded browser."
  (let ((url (concat "file://" (mu4e~write-body-to-html msg))))
    (xwidget-webkit-browse-url url)))

(defun search-for-sender (msg)
  "Search for MSG messages sent by the sender of the message at point."
  (mu4e-headers-search
    (concat "from:" (cdar (mu4e-message-field msg :from)))))

;; Variables
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq message-kill-buffer-on-exit t)
(setq mu4e-attachment-dir "~/Downloads")
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-compose-in-new-frame t)
(setq mu4e-compose-format-flowed t)
(setq mu4e-compose-signature-auto-include nil)
(setq mu4e-confirm-quit t)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "general"
        :enter-func (lambda () (mu4e-message "Entering general context"))
        :leave-func (lambda () (mu4e-message "Leaving general context"))
        :match-func (lambda (msg)
                      (when msg
                            (mu4e-message-contact-field-matches
                             msg '(:from :to :cc :bcc) "andrewwburch@gmail.com")))
        :vars '((user-mail-address . "andrewwburch@gmail.com")
                (user-full-name . "Andrew Burch")
                (mu4e-sent-folder . "/Sent")
                (mu4e-refile-folder . "/All")
                (mu4e-drafts-folder . "/Drafts")
                (mu4e-trash-folder . "/Trash")
                (mu4e-compose-signature . (concat "Cheers,\n Andrew"))
                (mu4e-compose-format-flowed . t)
                (smtpmail-queue-dir . "~/Mail/gmail/queue/cur")
                (message-send-mail-function . smtpmail-send-it)
                (smtpmail-smtp-user . "andrewwburch")
                (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                (smtpmail-default-smtp-server . "smtp.gmail.com")
                (smtpmail-smtp-server . "smtp.gmail.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-debug-info . t)
                (smtpmail-debug-verbose . t)))))
(setq mu4e-headers-auto-update t)
(setq mu4e-headers-date-format "%H:%M %d-%m-%Y")
;; (setq mu4e-html2text-command "html2text -utf8")
;; (setq mu4e-html2text-command 'my-render-html-message)
(setq mu4e-get-mail-command "mbsync -c ~/.config/emacs/mu4e/.mbsyncrc -a")
(setq mu4e-maildir (expand-file-name "~/Mail"))
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-update-interval 180)
(setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)
(setq mu4e-view-prefer-html nil)
(setq mu4e-view-show-images t)
(setq mu4e-view-show-addresses 't)
(setq smtpmail-queue-mail nil)

(add-to-list 'mu4e-view-actions '("xsearch for sender" . search-for-sender) t)
(add-to-list 'mu4e-view-actions '("Webkit" . mu4e-view-in-browser-webkit) t)
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Evaluations


;; Hooks
(add-hook 'mu4e-compose-mode-hook
          (defun compose-mail ()
            "Settings for mail composition."
            (use-hard-newlines -1)))

;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-headers-mode-hook
          (defun mu4e-change-head()
            (interactive)
            (setq mu4e-headers-fields `((:date . 22)
                                        (:flags . 6)
                                        (:from . 22)
                                        (:thread-subject . ,(- (window-body-width) 70))
                                        (:size . 7)))))
(add-hook 'mu4e-view-mode-hook
          (lambda()
            (local-set-key (kbd "<RET>") 'mu4e-view-browse-url-from-binding)
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)
            (toggle-truncate-lines)))

;; TODO: Make sure this is not needed and remove
;; (add-hook 'mu4e-compose-mode-hook 'visual-line-mode)
;; (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)



(provide 'init-mail)
;;; init-mail.el ends here
