;; Install other packages
(prelude-require-packages '(yasnippet
                            helm-c-yasnippet
                            rainbow-delimiters
                            csharp-mode
                            omnisharp
                            helm-swoop
                            cmake-mode
                            markdown-mode
                            expand-region
                            visual-regexp
                            zencoding-mode
                            json-mode
                            win-switch
                            ag
                            helm-ag
                            highlight-symbol
                            dash
                            helm-dash
                            soundcloud
                            multiple-cursors
                            clj-refactor
                            dsvn))

;; Enable prelude modules
(require 'prelude-helm)
(require 'prelude-helm-everywhere)
(require 'prelude-company)
(require 'prelude-key-chord)
(require 'prelude-c)
(require 'prelude-clojure)
(require 'prelude-coffee)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-js)
(require 'prelude-org)
(require 'prelude-python)
(require 'prelude-shell)
(require 'prelude-web)
(require 'prelude-xml)
(require 'prelude-yaml)

(setq prelude-clean-whitespace-on-save nil
      prelude-auto-save nil)

(defun personal-dir-path (path)
  "Return full path for path under the personal directory."
  (concat (expand-file-name "~/.emacs.d/personal") "/" path))

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

;; Setup some useful global keybinds
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c w") 'z-normalize-whitespace)
(global-set-key (kbd "C-c l") 'goto-line)

;; Make help more helpful (and less intrusive).
(global-set-key (kbd "C-c C-h") 'help-command)
(global-set-key (kbd "C-c C-h a") 'apropos)

;; Configure default buffer settings
(setq-default buffer-file-coding-system 'iso-latin-1-unix
              tab-width 4
              indent-tabs-mode t
              fill-column 100)

;; c-mode
(setq c-default-style
      '((c-mode . "bsd")
        (c++-mode . "bsd")
        (java-mode . "java")
        (other . "bsd")))
(defvaralias 'c-basic-offset 'tab-width)

;; whitespace-mode
(setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark trailing))
      whitespace-display-mappings
      '(
        (space-mark 32 [183] [46])
        (newline-mark 10 [182 10])
        (tab-mark 9 [8594 9] [92 9])
        ))

;; rainbow-delimiters
(rainbow-delimiters-mode)

;; helm-swoop
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

;; yasnippet
(setq yas-snippet-dirs (personal-dir-path "snippets"))
(yas-global-mode t)
(global-set-key (kbd "C-c y") 'helm-c-yas-complete)

;; expand-region
(global-set-key (kbd "C-h") 'er/expand-region)

;; recentf
(setq-default recentf-max-saved-items 50)

;; clojure-mode
(require 'clojure-mode)
(define-key clojure-mode-map (kbd "C-:") nil)
(define-key clojure-mode-map (kbd "C-;") 'clojure-toggle-keyword-string)

;; win-switch
(setq win-switch-idle-time (* 10 60) ;; Don't timeout so quickly.
      win-switch-other-window-first nil)
(global-set-key (kbd "C-x o") 'win-switch-dispatch)

;; ag
(global-set-key (kbd "C-x C-a") 'ag)

;; highlight-symbol
(setq-default highlight-symbol-idle-delay 0.7)

;; omnisharp
(require 'omnisharp)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))
(setq-default omnisharp-server-executable-path "~/src/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")

;; company
(defun is-unneeded-company-backend? (backend)
  (let ((unneeded-backends '(company-bbdb company-eclim company-semantic
                                          company-xcode company-ropemacs company-capf
                                          company-oddmuse company-clang)))
    (if (-contains? unneeded-backends backend) nil backend)))
(setq company-backends (-filter 'is-unneeded-company-backend? company-backends)
      company-idle-delay 0.25)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)

;; vc-mode
(setq vc-stay-local nil)

;; nxml-mode
(setq nxml-slash-auto-complete-flag 1)

;; zencoding-mode
(setq zencoding-indentation 2)

;; visual-regexp
(global-set-key (kbd "C-x C-r") 'vr/query-replace)

;; helm-dash
(global-set-key (kbd "C-c C-h d") 'helm-dash)
(global-set-key (kbd "C-c C-h g") 'helm-dash-at-point)
(setq helm-dash-min-length 2
      helm-dash-common-docsets '("PostgreSQL" "qt335" "Qt")
      helm-dash-browser-func 'eww)

;; browse-url configuration
(setq browse-url-browser-function 'browse-url-chromium)

;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; Personal Modules
(add-to-list 'load-path (personal-dir-path "modules"))
(require 'rw-ediff)
(require 'rw-vc)
(require 'rw-key-chord)
(require 'rw-smartparens)
(require 'rw-projectile)
(require 'rw-web-mode)
(require 'rw-helm)
(require 'rw-helm-ag)
(require 'rw-org)
(require 'rw-emms)
(require 'rw-minibuffer)
(require 'rw-beacon)

;; Hooks
(add-hook 'prog-mode-common-hook
          (lambda ()
            (subword-mode)
            (highlight-symbol-mode)))
(add-hook 'cmake-mode-hook
          (lambda ()
            (setq helm-dash-docsets '("CMake"))))
(add-hook 'css-mode-hook
          (lambda ()
            (subword-mode)
            (setq helm-dash-docsets '("CSS"))))
(add-hook 'sgml-mode-hook
          (lambda ()
            (subword-mode)
            (zencoding-mode)
            (setq sgml-basic-offset 2)))
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  tab-width 4
                  py-indent-offset 4
                  python-indent-offset 4
                  helm-dash-docsets '("Python_2"))))
(add-hook 'html-mode-hook
          (lambda ()
            (setq tab-width 2
                  indent-tabs-mode nil
                  helm-dash-docsets '("AngularJS" "Bootstrap 3" "Bootstrap_2" "Font_Awesome" "HTML"))))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2
                  indent-tabs-mode nil
                  helm-dash-docsets '("AngularJS" "Javascript" "Lo-Dash" "jQuery"))))
(add-hook 'js-mode-hook
          (lambda ()
            (setq tab-width 2
                  js-indent-level 2
                  indent-tabs-mode nil)))
(add-hook 'coffee-mode-hook
          (lambda ()
            (setq tab-width 2
                  coffee-tab-width 2
                  indent-tabs-mode nil)))
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq helm-dash-docsets '("Clojure"))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq tab-width 2
                  indent-tabs-mode nil
                  helm-dash-docsets '("Emacs Lisp"))))
(add-hook 'less-css-mode-hook
          (lambda ()
            (setq tab-width 2
                  less-css-indent-level 2
                  css-indent-offset 2
                  indent-tabs-mode nil)))
(add-hook 'csharp-mode-hook
          (lambda ()
            (omnisharp-mode)))
(add-hook 'java-mode-hook
          (lambda ()
            (setq helm-dash-docsets '("Android" "Java"))))
(add-hook 'web-mode-hook
          (lambda ()
            (setq tab-width 2
                  indent-tabs-mode nil
                  web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)))
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.pr[oi]\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Additional prelude configs
(prelude-install-search-engine "fogbugz" "https://fogbugz.zuerchertech.com/default.asp?" "FogBugz: ")
(define-key prelude-mode-map (kbd "C-c f") 'prelude-fogbugz)

(setq-default
 frame-title-format
 '((:eval
    (concat
     (projectile-project-name)
     " - Emacs")))

 header-line-format
 '((:eval
    (concat
     (projectile-project-name)
     " - "
     (if (buffer-file-name)
         (abbreviate-file-name (buffer-file-name))
       "%b")))))
(setq mode-line-misc-info
      (assq-delete-all 'which-func-mode mode-line-misc-info))

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))
