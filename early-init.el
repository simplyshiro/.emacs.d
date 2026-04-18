;;; early-init.el --- Early Init -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(defvar shiro--gc-cons-threshold gc-cons-threshold)
(defvar shiro--gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(defgroup shiro nil
  "Custom options for shiro's `init.el'."
  :tag "shiro"
  :group 'emacs
  :prefix "shiro-")

(defcustom shiro-variable-directory (expand-file-name "var/" user-emacs-directory)
  "Directory to store variable files."
  :tag "shiro Variable Directory"
  :group 'shiro
  :type 'directory)

(setopt backup-directory-alist
        `(("." . ,(expand-file-name "backups/" shiro-variable-directory))))

(startup-redirect-eln-cache
 (expand-file-name "eln-cache/" shiro-variable-directory))

(setopt custom-theme-directory
        (expand-file-name "themes/" shiro-variable-directory))
(setopt treesit--install-language-grammar-out-dir-history
        (list (expand-file-name "tree-sitter/" shiro-variable-directory)))
(setopt treesit-extra-load-path
        (list (expand-file-name "tree-sitter/" shiro-variable-directory)))

(setopt auto-save-list-file-prefix
        (expand-file-name "auto-save-list/.saves-" shiro-variable-directory))
(setopt transient-history-file
        (expand-file-name "transient/history.el" shiro-variable-directory))

(setopt custom-file (expand-file-name "custom.el" shiro-variable-directory))
(setopt savehist-file (expand-file-name "history" shiro-variable-directory))
(setopt save-place-file (expand-file-name "places" shiro-variable-directory))
(setopt project-list-file
        (expand-file-name "projects" shiro-variable-directory))
(setopt recentf-save-file
        (expand-file-name "recentf" shiro-variable-directory))
(setopt tramp-persistency-file-name
        (expand-file-name "tramp" shiro-variable-directory))

(setq package-enable-at-startup nil)

(setopt load-prefer-newer t)

(setopt inhibit-startup-buffer-menu t)
(setopt inhibit-startup-echo-area-message user-login-name)
(setopt inhibit-startup-screen t)
(setopt initial-buffer-choice nil)
(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message nil)

(setopt frame-inhibit-implied-resize t)
(setopt frame-resize-pixelwise t)

(when (boundp 'pgtk-wait-for-event-timeout)
  (setopt pgtk-wait-for-event-timeout 0.001))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(defun shiro-restore-gc-values ()
  "Restore garbage collection values."
  (setq gc-cons-threshold shiro--gc-cons-threshold)
  (setq gc-cons-percentage shiro--gc-cons-percentage))

(add-hook 'emacs-startup-hook #'shiro-restore-gc-values most-positive-fixnum)

(provide 'early-init)

;;; early-init.el ends here
