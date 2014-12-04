;;; rw-key-chord.el --- key-chord configuration

;;; Commentary:

;;; key-chord configuration

;;; Code:

(require 'key-chord)
;; key-chord-unset-global seems to have a bug so undefine it instead.
(key-chord-define-global "uu" 'nil)
(key-chord-define-global "xx" 'nil)

(provide 'rw-key-chord)

;;; rw-key-chord.el ends here
