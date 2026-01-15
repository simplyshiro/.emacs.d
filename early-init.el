;;; early-init.el --- Early Init -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d
;;
;;; Commentary:
;;
;;; Code:

(add-hook 'before-init-hook
          (lambda ()
            (setopt gc-cons-threshold most-positive-fixnum)))

(defun shiro-convert-from-mib-to-b (mebibytes)
  "Convert a number of MEBIBYTES to bytes."
  (* mebibytes (expt 2 20)))

(defvar shiro-variable-directory
  (expand-file-name "var/" user-emacs-directory))

(setopt backup-directory-alist
        `(("." . ,(expand-file-name "backups/" shiro-variable-directory))))

(when (featurep 'native-compile)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" shiro-variable-directory)))

(setopt custom-theme-directory
        (expand-file-name "themes/" shiro-variable-directory))
(setopt treesit-extra-load-path
        (list (expand-file-name "tree-sitter/" shiro-variable-directory)))

(setopt auto-save-list-file-prefix
        (expand-file-name "auto-save-list/.saves-" shiro-variable-directory))
(setopt eshell-history-file-name
        (expand-file-name "eshell/history" shiro-variable-directory))
(setopt eshell-last-dir-ring-file-name
        (expand-file-name "eshell/lastdir" shiro-variable-directory))
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

(when (featurep 'native-compile)
  (setopt package-native-compile t))

(setopt load-prefer-newer t)

(setopt inhibit-startup-buffer-menu t)
(setopt inhibit-startup-echo-area-message user-login-name)
(setopt inhibit-startup-screen t)
(setopt initial-buffer-choice nil)
(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message nil)

(when (boundp 'pgtk-wait-for-event-timeout)
  (setopt pgtk-wait-for-event-timeout nil))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (shiro-convert-from-mib-to-b 64))))

(provide 'early-init)

;;; early-init.el ends here
