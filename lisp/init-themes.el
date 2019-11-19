;;; init-themes.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'atom-one-dark-theme)
(maybe-require-package 'intellij-theme)
(maybe-require-package 'material-theme)
(when (maybe-require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;--------------------------------------------------------
;; highlight parentheses and make it for all buffers
;;--------------------------------------------------------
(when (maybe-require-package 'highlight-parentheses)
  (add-hook 'after-init-hook 'highlight-parentheses-mode)
  (add-hook 'after-init-hook 'show-paren-mode))

;; load better defaults
(maybe-require-package 'better-defaults)

;; (setq-default custom-enabled-themes '(material-light))

(defun reapply-themes ()
  "Forcibly load the theme listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote , custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;;---------------------------------------------------------------
;; Quick toggle theme
;;---------------------------------------------------------------
(defun atom-one-dark()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enable-themes '(atom-one-dark))
  (reapply-themes))

(defun intellij-theme()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(intellij))
  (reapply-themes))

(defun material-theme()
  "Activate a material-dark theme."
  (interactive)
  (setq custom-enabled-themes '(material))
  (reapply-themes))

(defun material-light-theme()
  "Activate a material-light theme."
  (interactive)
  (setq custom-enabled-themes '(material-light))
  (reapply-themes))

(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  (after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

(provide 'init-themes)
;;; init-themes.el ends here
