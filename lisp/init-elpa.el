;;; init-elpa.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'package)

(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
			 user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))


(let* ((no-ssl (and (memq system-type '(window-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))


(require 'cl-lib)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.  if NO-REFRESH is non-nil, the available package lists will not be re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
	     (versions (mapcar #'package-desc-version known)))
	(if (cl-find-if (lambda (v) (version-list-<= min-version v)) versions)
	    (package-install package)
	  (if no-refresh
	      (error "No version of %s >= $S is available" package min-version)
	    (package-refresh-contents)
	    (require-package package min-version t))))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.  In the event of failure, return nil and print a warning message.  Optionally require MIN-VERSION.  if NO-REFRESH is non-nil, the available package lists will not be re-installed in order to locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optinal package `%s':%S" package err)
     nil)))

;;; fire up package.el
(setq package-enable-at-startup nil)
(package-initialize)

(provide 'init-elpa)

;;; init-elpa.el ends here
