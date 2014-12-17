;;; rw-emms.el --- emms configuration

;;; Commentary:

;;; emms configuration

;;; Code:

(require 'emms-setup)
(emms-standard)
(setq emms-player-list '(emms-player-vlc))
(define-emms-simple-player vlc '(file url)
  (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
				".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "https://" "mms://"
				".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
  "vlc")

(provide 'rw-emms)

;;; rw-emms.el ends here
