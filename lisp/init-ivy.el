;;; init-ivy.el --- Insert description here -*- lexical-binding: t --*
;;; Commentary:
;;; Code:


(when (maybe-require-package 'ivy)
  (add-hook 'after-init-hook 'ivy-mode)
  (after-load 'ivy
	 (setq-default ivy-use-virtual-buffers t
		       ivy-virtual-abbreviate 'fullpath
		       ivy-count-format ""
		       ivy-magic-tilde nil
		       ivy-dynamic-exhibit-delay-ms 150
		       ivy-use-selectable-prompt t
		       projectile-completion-system 'ivy)

	 (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
	 (dolist (k '("C-j" "C-RET"))
	   (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))
	   (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
	   (define-key ivy-occur-mode-map (kbd "C-c C-q") #'ivy-wgrep-chage-to-wgrep-mode)

	   (when (maybe-require-package 'diminish)
	     (diminish 'ivy-mode)))

	 (defun dingdang/enable-ivy-fix-matching ()
	   "Make `ivy' matching work mode like IDO."
	   (interactive)
	   (require-package 'fix)
	   (setq-default ivy-re-builders-alist
			 '((t . ivy--regex-fuzzy)))))

  (when (maybe-require-package 'counsel)
    (setq-default counsel-mode-override-describe-bindings t)
    (after-load 'counsel
      (setq-default ivy-initial-inputs-alist
		    '((Man-completion-table . "^")
		      (woman . "^"))))
    (when (maybe-require-package 'diminish)
      (after-load 'counsel
	(diminish 'counsel-mode)))
    (add-hook 'after-init-hook 'counsel-mode)

    (when (maybe-require-package 'projectile)
      (let ((search-function
	     (cond
	      ((executable-find "rg") 'counsel-rg)
	      ((executable-find "ag") 'counsel-ag)
	      ((executable-find "pt") 'counsel-pt)
	      ((executable-find "ack") 'counsel-ack))))
	(when search-function
	  (defun dingdang/counsel-search-project (initial-input &optional use-current-dir)
	    "Search using `counsel-rg' or similar from the project root for INITIAL-INPUT. If there is no project root, or if the prefix argument USAE-CURRENT-DIR is set, then search from the current directory instead."
	    (interactive (list (let ((sym (thing-at-point 'symbol)))
				 (when sym (regexp-quote sym)))
			       current-prefix-arg))
	    (let ((current-prefix-arg)
		  (dir (if use-current-dir
			   default-directory
			 (condition-case err
			     (projectile-project-root)
			   (error default-directory)))))
	      (funcall search-function initial-input dir)))))
      (after-load 'ivy
	(add-to-list 'ivy-height-alist (cons 'counsel-ag 20)))
      (global-set-key (kbd "M-?") 'dingdang/counsel-search-project)))

  (when (maybe-require-package 'swiper)
    (after-load 'ivy
      (define-key ivy-mode-map (kbd "M-s /") 'swiper-thing-at-point)))

  (when (maybe-require-package 'ivy-xref)
    (setq xref-shwo-xref-function 'ivy-xref-show-xrefs))

  (provide 'init-ivy)

  
;;; init-ivy.el ends here
