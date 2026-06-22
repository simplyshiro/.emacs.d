;;; early-init.el --- Early Init -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(defvar shiro--gc-cons-threshold
  (default-toplevel-value 'gc-cons-threshold))

(defvar shiro--gc-cons-percentage
  (default-toplevel-value 'gc-cons-percentage))

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(defun shiro-restore-gc-cons-values ()
  "Restore `gc-cons-threshold' and `gc-cons-percentage' to their default values."
  (setq gc-cons-threshold shiro--gc-cons-threshold)
  (setq gc-cons-percentage shiro--gc-cons-percentage))

(add-hook 'emacs-startup-hook #'shiro-restore-gc-cons-values 110)

(defvar shiro--file-name-handler-alist
  (default-toplevel-value 'file-name-handler-alist))

(setq file-name-handler-alist nil)

(defun shiro-restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist'."
  (set-default-toplevel-value 'file-name-handler-alist
                              (delete-dups
                               (append file-name-handler-alist
                                       shiro--file-name-handler-alist))))

(unless noninteractive
  (add-hook 'emacs-startup-hook #'shiro-restore-file-name-handler-alist 105))

(setq load-prefer-newer t)

(defvar root-emacs-directory user-emacs-directory)
(setq user-emacs-directory (expand-file-name "var/" root-emacs-directory))
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" user-emacs-directory))
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq read-process-output-max (expt 1024 2))

(unless noninteractive
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-echo-area-message user-login-name)
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice nil)
  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)

  (setq frame-inhibit-implied-resize t)
  (setq frame-resize-pixelwise t)

  (when (boundp 'pgtk-wait-for-event-timeout)
    (setq pgtk-wait-for-event-timeout 0.001))

  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

(setq package-enable-at-startup nil)

(provide 'early-init)

;;; early-init.el ends here
