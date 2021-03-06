;;; init-python.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConstruct\\'" . python-mode))
	      auto-mode-alist))

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (after-load 'python
	(push 'company-anaconda company-backends)))))


(provide 'init-python)
;;; init-python.el ends here
