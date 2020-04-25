;;; init-mail.el --- Emacs configuration for gmail.
;;; Commentary:
;;; Install mu4e, isync (for mbsync), gpg2, and libxml2 via apt.
;;; Encrypt password files and place them in designated folders with designated names
;;; Note mbsync configuration is in this repository under mu4e/.mbsyncrc
;;; Requires Emacs to be compiled with libxml2 (see configure.ac in Emacs source for details)
;;; Code:
(require 'init-elpa)

(require-package 'org-mime)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")

(require 'mu4e)
(require 'org-mu4e)
(require 'smtpmail)

(setq mu4e-maildir (expand-file-name "~/Mail"))

(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
      mu4e-view-prefer-html t
      mu4e-update-interval 180
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil
      mu4e-compose-format-flowed t)

(setq mu4e-view-show-images t)
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-compose-in-new-frame t)

(setq mu4e-sent-messages-behavior 'delete)

(add-hook 'mu4e-view-mode-hook 'visual-line-mode)
;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
          (lambda()
            (local-set-key (kbd "<RET>") 'mu4e-view-browse-url-from-binding)
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(add-hook 'mu4e-headers-mode-hook
          (defun mu4e-change-head()
            (interactive)
            (setq mu4e-headers-fields `((:date . 25)
                                        (:flags . 6)
                                        (:from . 22)
                                        (:thread-subject . ,(-(window-body-width) 70))
                                        (:size . 7)))))
                  

(setq mu4e-headers-date-format "%H:%M %d-%m-%Y")

(add-hook 'mu4e-compose-mode-hook
          (defun compose-mail ()
            "Settings for mail composition."
            (visual-line-mode)
            (org-mu4e-compose-org-mode)
            (use-hard-newlines -1)
            (flyspell-mode)))

(setq mu4e-change-filenames-when-moving t)
(setq smtpmail-queue-mail nil)
(setq mu4e-attachment-dir "~/Downloads")
(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)

(setq org-mu4e-convert-to-html t)
(setq mu4e-view-show-addresses 't)
(setq mu4e-confirm-quit t)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
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

(defun my-render-html-message ()
  "Rendering email."
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

(defun mu4e-my-show-in-browser ()
  "Show the daggum email."
  (interactive)
  (mu4e-action-view-in-browser (mu4e-action-view-in-browser (mu4e-message-at-point t))))

(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
;; (setq mu4e-html2text-command "html2text -utf8")
(setq browse-url-browser-function 'eww-browse-url)
(setq mu4e-html2text-command 'my-render-html-message)

(provide 'init-mail)
;;; init-mail.el ends here
