;;; init-whitespace.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default show-trailing-whitespace nil)

(defun dingdang/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode conf-mode-hook))
  (add-hook hook 'dingdang/show-trailing-whitespace))

(require-package 'whitespace-cleanup-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
(after-load 'whitespace-cleanup-mode
  (diminish 'whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)

(provide 'init-whitespace)
;;; init-whitespace.el ends here
