;;; init-windows.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(add-hook 'after-init-hook 'winner-mode)

(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)

;;------------------------------------------------------------;;
;; when splitting window, show other buffer in the new window ;;
;;------------------------------------------------------------;;
(defun split-window-func-with-other-buffer(split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))


(defun dingdang/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
      (equal (selected-window) (next-window)))
      (winner-undo)
  (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'dingdang/toggle-delete-other-windows)


;;-------------------------------------------------------------;;
;;rearrang split windows                                       ;;
;;-------------------------------------------------------------;;
(defun split-window-horizontally-new ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-new ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))


(global-set-key (kbd "C-x |") 'split-window-horizontally-new)
(global-set-key (kbd "C-x -") 'split-window-vertically-new)


(defun dingdang/split-window ()
  "Split the window to see the most recent buffer in the other window.  Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'dingdang/split-window)
      (progn
	(jump-to-register :dingdang/split-window)
	(setq this-command 'dingdang/split-window))
    (window-configuration-to-register :dingdang/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'dingdang/split-window)

(defun dingdang/toggle-current-window-dedication()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
	 (was-dedicated (window-dedicated-p window)))
       (set-window-dedicated-p window (not was-dedicated))
       (message "Window %sdedicated to %s"
		(if was-dedicated "no longer " "")
		(buffer-name))))

(global-set-key (kbd "C-c <down>") 'dingdang/toggle-current-window-dedication)

(provide 'init-windows)
;;; init-windows.el ends here
