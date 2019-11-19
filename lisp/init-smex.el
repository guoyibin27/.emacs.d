;;; init-smex.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'ido)
(maybe-require-package 'ido-completing-read+)
(maybe-require-package 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.history" user-emacs-directory)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)
(ido-ubiquitous-mode +1)
(flx-ido-mode +1)
(setq ido-use-faces nil)

(when (maybe-require-package 'smex)
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

(provide 'init-smex)
;;; init-smex.el ends here
