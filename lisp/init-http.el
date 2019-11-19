;;; init-http.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(maybe-require-package 'httprepl)

(when (maybe-require-package 'restclient)
  (add-auto-mode 'restclient-mode "\\.rest\\'")

  (defun dingdang/restclient ()
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'init-http)
;;; init-http.el ends here
