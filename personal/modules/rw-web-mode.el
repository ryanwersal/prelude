;;; rw-web-mode.el --- web-mode configuration

;;; Commentary:

;;; web-mode configuration

;;; Code:

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(setq web-mode-engines-alist
      '(("razor" . "\\.cshtml\\'")))

;; Undefine C-c C-h since it conflicts with the global help bind.
(define-key web-mode-map (kbd "C-c C-h") nil)

(provide 'rw-web-mode)

;;; rw-web-mode.el ends here

