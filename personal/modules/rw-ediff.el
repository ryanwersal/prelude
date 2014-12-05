;;; rw-ediff.el --- ediff configuration

;;; Commentary:

;;; ediff configuration

;;; Code:

(require 'ediff)
(setq ediff-split-window-function 'split-window-horizontally)

(defvar ediff-saved-window-config nil "Saved window configuration.")
(defun ediff-save-window-config ()
  "Save current window configuration."
  (setq ediff-saved-window-config (current-window-configuration)))
(defun ediff-restore-window-config ()
  "Restore previous window configuration."
  (set-window-configuration ediff-saved-window-config))

(add-hook 'ediff-before-setup-hook 'ediff-save-window-config)
(add-hook 'ediff-quit-hook 'ediff-restore-window-config 'append)
(add-hook 'ediff-suspend-hook 'ediff-restore-window-config 'append)

(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'vc-ediff))
(eval-after-load "vc-dir"
  '(define-key vc-dir-mode-map "=" 'vc-ediff))

(provide 'rw-ediff)

;;; rw-ediff.el ends here
