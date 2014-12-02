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

(delete-selection-mode t)
(global-auto-revert-mode 1)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell t)

(set-default 'sentence-end-double-space nil) ;; Don't require double spaces after periods anywhere.
