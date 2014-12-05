;;; rw-web-mode.el --- web-mode configuration

;;; Commentary:

;;; web-mode configuration

;;; Code:

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(setq web-mode-engines-alist
      '(("razor" . "\\.cshtml\\'")))

(provide 'rw-web-mode)

;;; rw-web-mode.el ends here

