;;; rw-org.el --- org configuration

;;; Commentary:

;;; org configuration

;;; Code:

(require 'org)
(require 'org-capture)

;; Configure org-capture
(setq org-directory "~/Dropbox/org"
      org-default-notes-file "~/Dropbox/org/refile.org")

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org/gtd.org") "* TODO: %?\n%U\n")
              ("c" "code note" entry (file "~/Dropbox/org/gtd.org") "* CODE: %?\n%U\n%a\n")
              ("p" "paste" entry (file "~/Dropbox/org/gtd.org") "* PASTE: %?\n%U\n%x\n"))))

;; Add support for adding links to man pages
(org-add-link-type "man" 'org-man-open)
(add-hook 'org-store-link-functions 'org-man-store-link)

(defcustom org-man-command 'man
  "Emacs command to be used to display a man page."
  :group 'org-link
  :type '(choice (const man) (const woman)))

(defun org-man-open (path)
  "Visit the manpage on PATH.  PATH should be a topic that can be passed to the man command."
  (funcall org-man-command path))

(defun org-man-store-link ()
  "Store a link to a manpage."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, we do make this link
    (let* ((page (org-man-get-page-name))
           (link (concat "man:" page))
           (description (format "Manpage for %s" page)))
      (org-store-link-props
       :type "man"
       :link link
       :description description))))

(defun org-man-get-page-name ()
  "Extract the page name from the buffer name."
  ;; This works for both `Man-mode' and `woman-mode'
  (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot create link to this man page")))

(provide 'rw-org)

;;; rw-org.el ends here
