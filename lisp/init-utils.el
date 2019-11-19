;;; init-utils.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(if (fboundp 'with-eval-after-laod)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "after feature is loaded, eval body."
    (declare (indent defun))
    `(eval-after-load ,feature '(progn ,@body))))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; delete the current file
(defun delete-this-file ()
  "Delete the current file , and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
			     (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; rename the current file
(defun rename-this-file-and-buffer (new-name)
  "Rename both current file and buffer with NEW-NAME."
  (interactive "sNew name:")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
	(rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; browse current HTML file
(defun browse-current-html-file ()
  "Open the current file as URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
	     (tramp-tramp-file-p file-name))
      (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(provide 'init-utils)
;;; init-utils.el ends here
