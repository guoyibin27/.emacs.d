;;; init-recentf.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/","/ssh:"))

(provide 'init-recentf)
;;; init-recentf.el ends here
