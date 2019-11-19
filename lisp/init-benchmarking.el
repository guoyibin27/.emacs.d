;;; init-benchmarking.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun dingdang/time-subtract-millis (b a)
  "Subtract B and A."
  (* 1000.0 (float-time (time-subtract b a))))


(defun dingdang/show-init-time ()
  "Show Emacs init time."
  (message "Init completed in %.2fms"
	   (dingdang/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'dingdang/show-init-time)

(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
