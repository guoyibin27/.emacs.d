;;; init-lisp.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require-package 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))

(setq-default initial-scratch-message
	      (concat "; Welcome " user-login-name " to Emacs world!\n\n"))

(defun dingdang/headerise-elisp()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
		   (file-name-nondirectory (buffer-file-name))
		 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
	      ";;; Commentary:\n"
	      ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))

(defun dingdang/eval-last-sexp-or-region (prefix)
  "Eval region (PREFIX) from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'dingdang/eval-last-sexp-or-region))

(when (maybe-require-package 'ipretty)
  (add-hook 'after-init-hook 'ipretty-mode))


(defun dingdang/make-buffer-readonly (expression out-buffer-name)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))
(advice-add 'pp-display-expression :after 'dingdang/make-buffer-readonly)

(defun dingdang/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
	     (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))
(add-hook 'emacs-lisp-mode-hook 'dingdang/maybe-set-bundled-elisp-readonly)
  
;; Use C-c C-z to toggle between elisp files and an ielm session
(defvar-local dingdang/repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")

(defvar dingdang/repl-switch-function 'switch-to-buffer-other-window)

(defun dingdang/switch-to-ielm()
  "Switch to the ielm buffer."
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm")
	(funcall dingdang/repl-switch-function "*ielm*")
      (ielm))
    (setq dingdang/repl-original-buffer orig-buffer)))

(defun dingdang/repl-switch-back()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if dingdang/repl-original-buffer
      (funcall dingdang/repl-switch-function dingdang/repl-original-buffer)
    (error "No original buffer")))

(after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'dingdang/switch-to-ielm))
(after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'dingdang/repl-switch-back))


;;------------------------------------
;; automatic byte compilation
;;------------------------------------
(when (maybe-require-package 'auto-compile)
  (add-hook 'after-init-hook 'auto-compile-on-save-mode)
  (add-hook 'after-init-hook 'auto-compile-on-load-mode))


;;-------------------------------------------------
;; load .el if newer than corresponding .elc
;;-------------------------------------------------
(setq load-prefer-newer t)

(require-package 'immortal-scratch)
(add-hook 'after-init-hook 'immortal-scratch-mode)

;; support byte-compilation in a sub-process, as required by highlight-cl
(defun dingdang/byte-compile-file-batch(filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode:")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" file)
       " ")))))

;;-------------------------------------------------------------
;; hippie-expand
;;------------------------------------------------------------
(defun set-up-hippie-expand-for-elisp()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

;;--------------------------------------------------------------
;;Enable desired features for all list modes
;;--------------------------------------------------------------
(defun dingdang/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar dingdang/lispy-modes-hook
  '(enable-paredit-mode
    dingdang/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(when (maybe-require-package 'aggressive-indent)
  (add-to-list 'dingdang/lispy-modes-hook 'aggressive-indent-mode))

(defun dingdang/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'dingdang/lispy-modes-hook))

(defun dingdang/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst dingdang/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst dingdang/lispy-modes
  (append dingdang/elispy-modes
	  '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name dingdang/lispy-modes))
  (add-hook hook 'dingdang/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name dingdang/elispy-modes))
  (add-hook hook 'dingdang/emacs-lisp-setup))

(when (boundp 'eval-expression-minibuffer-setup-hook)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))
	    
(require-package 'cl-lib-highlight)
(after-load 'lisp-mode
  (cl-lib-highlight-initialize))

(global-set-key (kbd "C-c k") 'find-function-on-key)


;; extras for theme editing
(when (maybe-require-package 'rainbow-mode)
  (defun dingdang/enable-rainbow-mode-if-theme()
    (when (and (buffer-file-name) (string-match-p "\\(color-theme-\\|-theme\\.el" (buffer-file-name)))
	       (rainbow-mode)))
    (add-hook 'emacs-lisp-mode-hook 'dingdang/enable-rainbow-mode-if-theme)
    (add-hook 'help-mode-hook 'rainbow-mode)
    (after-load 'rainbow-mode
      (diminish 'rainbow-mode)))

(when (maybe-require-package 'highlight-quoted)
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))


(when (maybe-require-package 'flycheck)
    (require-package 'flycheck-package)
    (after-load 'flycheck
      (after-load 'elisp-mode
	(flycheck-package-setup))))

(after-load 'ert
  (define-key ert-results-mode-map (kbd "g") 'ert-results-rerun-all-tests))

(maybe-require-package 'cl-libify)
(maybe-require-package 'cask-mode)

(provide 'init-lisp)
;;; init-lisp.el ends here
