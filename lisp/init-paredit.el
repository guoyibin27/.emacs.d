;;; init-paredit.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require-package 'paredit)

(defun maybe-map-paredit-newline()
  "Place newline if paredit needed."
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
	      (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)

(after-load 'paredit
  (diminish 'paredit-mode " Par")
  (dolist (binding '("C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil)))


(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
(defvar paredit-minibuffer-commands '(eval-expression
				      pp-eval-expression
				      eval-expression-with-eldoc
				      ibuffer-do-eval
				      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable ParEdit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

(require-package 'paredit-everywhere)
(after-load 'paredit-everywhere
  (define-key paredit-everywhere-mode-map (kbd "M-s") nil))
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(provide 'init-paredit)
;;; init-paredit.el ends here
