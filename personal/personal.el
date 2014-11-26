;; Install other packages
(prelude-require-packages '(maxframe
                            yasnippet
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
                            tagedit))

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

;; maxframe
(add-hook 'window-setup-hook 'maximize-frame t)

;; helm
(setq helm-idle-delay 0.1
      helm-input-idle-delay 0.1)
(global-set-key (kbd "C-x C-f") 'helm-for-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; helm-swoop
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

;; yasnippet
(setq yas/snippet-dirs (personal-dir-path "snippets"))
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

;; web-mode
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(setq web-mode-engines-alist
      '(("razor" . "\\.cshtml\\'")))

;; Hooks
(add-hook 'prog-mode-common-hook
          (lambda ()
            (subword-mode)
            (higlight-fixme-tokens)
            (highlight-symbol-mode)))
(add-hook 'css-mode-hook
          (lambda ()
            (subword-mode)
            (highlight-fixme-tokens)))
(add-hook 'sgml-mode-hook
          (lambda ()
            (subword-mode)
            (zencoding-mode)
            (tagedit-mode 1)
            (setq sgml-basic-offset 2)))
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4
                  py-indent-offset 4
                  python-indent 4)))
(add-hook 'html-mode-hook
          (lambda ()
            (setq tab-width 2
                  indent-tabs-mode nil)))
(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2
                  indent-tabs-mode nil)))
(add-hook 'js-mode-hook
          (lambda ()
            (setq tab-width 2
                  js-indent-level 2
                  indent-tabs-mode nil)))
(add-hook 'coffee-mode-hook
          (lambda ()
            (setq tab-width 2
                  coffee-tab-width 2
                  indent-tab-mode nil)))
(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)))
(add-hook 'less-css-mode-hook
          (lambda ()
            (setq tab-width 2
                  less-css-indent-level 2
                  css-indent-offset 2
                  indent-tabs-mode nil)))
(add-hook 'csharp-mode-hook
          (lambda ()
            (omnisharp-mode)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.pr[oi]\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Additional prelude configs
(prelude-install-search-engine "fogbugz" "https://fogbugz.zuerchertech.com/default.asp?" "FogBugz: ")
(define-key prelude-mode-map (kbd "C-c f") 'prelude-fogbugz)

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))
