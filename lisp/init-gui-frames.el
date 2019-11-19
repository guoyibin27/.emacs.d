;;; init-gui-frames.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(global-hl-line-mode t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode t)
(global-linum-mode t)
(blink-cursor-mode -1)
(set-cursor-color "#000000")
(setq ring-bell-function 'ignore)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(maybe-require-package 'beacon)
(require 'beacon)
(beacon-mode +1)

(maybe-require-package 'which-key)
(which-key-mode +1)


(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(add-hook 'term-mode-hook
	  (lambda ()
	    (setq line-spacing 0)))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-by-copying nil)
(setq large-file-warning-threshold nil)
(setq-default fill-column 120)

(when (maybe-require-package 'window-numbering)
  (add-hook 'after-init-hook 'window-numbering-mode t))

(when (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "M-f") 'toggle-frame-fullscreen))

(when (maybe-require-package 'default-text-scale)
  (add-hook 'after-init-hook 'default-text-scale-mode))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
