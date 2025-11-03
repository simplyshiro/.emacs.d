;;; early-init.el --- Early Init -*- lexical-binding: t; no-byte-compile: t; no-native-compile: t; -*-

;;; Commentary:

;;; Code:

(defun my-convert-from-mebibytes-to-bytes (mebibytes)
  "Convert MEBIBYTES to bytes."
  (* mebibytes 1048576))

(setopt load-prefer-newer t)

(setopt gc-cons-threshold most-positive-fixnum)

(setopt initial-buffer-choice nil)
(setopt inhibit-startup-screen t)
(setopt inhibit-startup-echo-area-message user-login-name)
(setopt inhibit-startup-buffer-menu t)
(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message nil)

(setopt menu-bar-mode nil)
(setopt scroll-bar-mode nil)
(setopt tool-bar-mode nil)

(setq package-enable-at-startup nil)

(when (native-comp-available-p)
  (setopt package-native-compile t))

(setopt use-package-hook-name-suffix nil)
(setopt use-package-always-ensure t)

(setopt user-emacs-directory (expand-file-name "var/" user-emacs-directory))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (my-convert-from-mebibytes-to-bytes 64))))

(provide 'early-init)

;;; early-init.el ends here
