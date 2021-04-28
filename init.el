
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:
(require 'org)
(require 'package)

(defvar current-date-time-format "%a %b %d %Y-%m-%dT%H:%M:%S "
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

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

(defun my-tangle-config-org ()
  "This function will write source blocks from =config.org= into =config.el=.
- not marked as =tangle: no=
- doesn't have the TODO state =DISABLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (output-file (concat user-emacs-directory "config.el"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "—————• Re-generating %s …" output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks (concat user-emacs-directory "config.org")
	  (let* (
		 (org_block_info (org-babel-get-src-block-info 'light))
		 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
		 (match_for_TODO_keyword)
		 )
	    (save-excursion
	      (catch 'exit
		(org-back-to-heading t)
		(when (looking-at org-outline-regexp)
		  (goto-char (1- (match-end 0))))
		(when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
		  (setq match_for_TODO_keyword (match-string 1)))))
	    (unless (or (string= "no" tfile)
			(string= "DISABLED" match_for_TODO_keyword)
			(not (string= "emacs-lisp" lang)))
	      (add-to-list 'body-list (concat "\n;; #####################################################################################\n"
					      "(message \"config • " (org-get-heading) " …\")\n")
			   )
	      (add-to-list 'body-list body)
	      ))))
      (with-temp-file output-file
        (insert ";;; config.el --- Summary\n")
        (insert ";;; Commentary:\n")
        (insert ";;; Don't edit this file, edit config.org' instead ...\n")
        (insert (format ";;; Auto-generated at %s\n" (format-time-string current-date-time-format (current-time))))
        (insert ";;; Code\n")
        (insert (apply 'concat (reverse body-list)))
        (insert "\n(provide 'config)\n")
        (insert ";;; config.el ends here"))
      (message "—————• Wrote %s" output-file))))

(defun my-tangle-config-dependencies ()
  "This function will write source blocks from =config.org= into =dependencies.sh=.
- not marked as =tangle: no=
- have a source-code of =shell="
  (require 'org)
  (let* ((body-list ())
         (output-file (concat user-emacs-directory "dependencies.sh"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "—————• Re-generating %s …" output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks (concat user-emacs-directory "config.org")
	  (let* (
		 (org_block_info (org-babel-get-src-block-info 'light))
		 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
		 (match_for_TODO_keyword)
		 )
	    (save-excursion
	      (catch 'exit
		(org-back-to-heading t)
		(when (looking-at org-outline-regexp)
		  (goto-char (1- (match-end 0))))))
	    (unless (or (string= "no" tfile)
			(not (string= "shell" lang)))
	      (add-to-list 'body-list body)
	      ))))
      (with-temp-file output-file
        (insert "#!/bin/bash\n")
        (insert "# dependencies.sh\n")
        (insert "# Don't edit this file, edit config.org' instead ...\n")
        (insert (format "# Auto-generated at %s\n" (format-time-string current-date-time-format (current-time))))
        (insert (apply 'concat (reverse body-list))))
      (message "—————• Wrote %s" output-file))))

;; when config.org is saved, re-generate config.el:
(defun my-tangle-config-org-hook-func ()
  (when (string= "config.org" (buffer-name))
	(let ((orgfile (concat user-emacs-directory "config.org"))
	      (elfile (concat user-emacs-directory "config.el"))
          (shfile (concat user-emacs-directory "dependencies.sh")))
	  (my-tangle-config-org)
          (my-tangle-config-dependencies))))

(defun my-tangle-config-dependency-hook-func ()
  (when (string= "config.org" (buffer-name))
    (let ((orgfile (concat user-emacs-directory "config.org"))
          (shfile (concat user-emacs-directory "dependencies.sh")))
      (my-tangle-config-dependencies))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((orgfile (concat user-emacs-directory "config.org"))
      (elfile (concat user-emacs-directory "config.el"))
      (gc-cons-threshold most-positive-fixnum))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (my-tangle-config-org))
  (load-file elfile))
(add-hook 'after-save-hook 'my-tangle-config-org-hook-func)

(provide 'init)
;;; init.el ends here
