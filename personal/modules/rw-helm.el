;;; rw-helm.el --- helm configuration

;;; Commentary:

;;; helm configuration

;;; Code:


;; helm
(setq helm-idle-delay 0.1
      helm-input-idle-delay 0.1
      helm-M-x-fuzzy-match t)
(global-set-key (kbd "C-x C-f") 'helm-for-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(provide 'rw-helm)

;;; rw-helm.el ends here
