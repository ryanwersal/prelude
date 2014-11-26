;; Configure theme
(setq custom-theme-directory "~/.emacs.d/personal/themes/"
      prelude-theme 'base16-dark)

;; Detect OS
(defvar is-windows-p (eq system-type 'windows-nt))
(defvar is-osx-p (eq system-type 'darwin))
(defvar is-linux-p (eq system-type 'gnu/linux))

;; Set default font per platform
(defvar default-font-name
  (cond (is-windows-p "PragmataPro-8")
        (is-osx-p "PragmataPro-12")
        (is-linux-p "PragmataPro-9")
        (t "PragmataPro-10")))
(add-to-list 'default-frame-alist `(font .,default-font-name))

;; Configure default buffer settings
(setq-default buffer-file-coding-system 'iso-latin-1-unix
              tab-width 4
              indent-tabs-mode t
              fill-column 100)

(delete-selection-mode t)
(global-auto-revert-mode 1)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell t)

(set-default 'sentence-end-double-space nil) ;; Don't require double spaces after periods anywhere.

(defun personal-dir-path (path)
  "Return full path for path under the personal directory."
  (concat (expand-file-name "~/.emacs.d/personal") "/" path))

(defun highlight-fixme-tokens ()
  "Highlight fixme tokens in comments."
  (font-lock-add-keywords nil '(("\\<\\(IMPROVEME\\|FIXME\\|TODO\\|BUG\\|NOTE\\|HACK\\)[:\(]" 1
                                 font-lock-warning-face t))))

(defun confirm-exit ()
  "Prompt prior to exit."
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
      (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'confirm-exit)
(global-unset-key (kbd "C-z")) ;; Nuke suspend key

(defun z-normalize-newlines ()
  "Clean up line endings."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix))

(defun z-normalize-whitespace ()
  "Clean up whitespace."
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end)))
    (whitespace-cleanup-region beg end)
    (cond (indent-tabs-mode (tabify beg end))
          (t (untabify beg end)))))

(defun eval-and-replace ()
  "Replace preceding sexp with its result."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun open-fogbugz ()
  (interactive)
  (browse-url
   (concat
    "https://fogbugz.zuerchertech.com/default.asp?"
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "FogBugz #: "))))))

;; Setup some useful global keybinds
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-c C-f") 'open-fogbugz)
(global-set-key (kbd "C-x i") 'imenu)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c w") 'z-normalize-whitespace)
(global-set-key (kbd "C-c l") 'goto-line)

;; Make help more helpful (and less intrusive).
(global-set-key (kbd "C-c C-h") 'help-command)
(global-set-key (kbd "C-c C-h a") 'apropos)
