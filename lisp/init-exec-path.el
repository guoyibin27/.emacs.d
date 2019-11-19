;;; init-exec-path.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require-package 'exec-path-from-shell)

(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))


(provide 'init-exec-path)
;;; init-exec-path.el ends here
