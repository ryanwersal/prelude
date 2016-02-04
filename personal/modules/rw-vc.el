;;; rw-vc.el --- vc-mode/-dir/dsvn configuration

;;; Commentary:

;;; vc-mode/-dir/dsvn configuration

;;; Code:

(add-hook 'vc-dir-mode-hook
          (lambda ()
            (vc-dir-hide-state 'needs-update)
            (vc-dir-hide-state 'unregistered)
            (vc-dir-hide-state 'ignored)))

(require 'dsvn)

(provide 'rw-vc)

;;; rw-vc.el ends here
