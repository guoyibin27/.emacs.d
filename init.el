;;; init.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;-------------------------------
;; some basic configuration      ;;
;;-------------------------------;;
(global-set-key (kbd "<f5>") 'eval-buffer)
(defconst *is-a-mac* (eq system-type 'darwin))
(global-set-key (kbd "C-x TAB") 'previous-buffer)
(global-set-key (kbd "C-x <backtab>") 'switch-to-next-buffer)

;;-------------------------------;;
;; bootstrap configuration       ;;
;;-------------------------------;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-utils)
(require 'init-elpa)
(require 'init-exec-path)
(require 'init-benchmarking)

;;-----------------------------;;
;; load other configuration    ;;
;;-----------------------------;;
(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-themes)
(require 'init-gui-frames)
(require 'init-recentf)
(require 'init-ivy)
(require 'init-dired)
(require 'init-company)
(require 'init-ibuffer)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)
(require 'init-folding)
(require 'init-smex)
(require 'init-frame-hooks)
(require 'init-paredit)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-whitespace)
(require 'init-isearch)
(require 'init-uniquify)


(require 'init-lisp)
(require 'init-common-lisp)
(require 'init-html)
(require 'init-html)
(require 'init-python)
(require 'init-yaml)


(require 'init-locales)
(require 'init-local nil t)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))
(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(provide 'init)
;;; init.el ends here
