;;; early-init.el --- Early Init -*- lexical-binding: t; no-byte-compile: t; no-native-compile: t; -*-

;;; Commentary:

;;; Code:

(defconst my-mebibytes-in-bytes (expt 2 20)
  "The number of bytes in one mebibyte.")

(defun my-convert-mebibytes-to-bytes (mebibytes)
  "Convert MEBIBYTES to bytes."
  (* mebibytes my-mebibytes-in-bytes))

(setopt gc-cons-threshold most-positive-fixnum)

(when (native-comp-available-p)
  (setopt package-native-compile t))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (my-convert-mebibytes-to-bytes 32))))

(setopt load-prefer-newer t)

(setopt user-emacs-directory (expand-file-name "var/" user-emacs-directory))

(set-language-environment "UTF-8")

;; `set-language-environment' overrides this value, set it back to nil.
(setopt default-input-method nil)

(setopt inhibit-compacting-font-caches t)

(setopt frame-resize-pixelwise t)
(setopt frame-inhibit-implied-resize t)

(setopt inhibit-startup-screen t)
(setopt inhibit-startup-echo-area-message user-login-name)

(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message nil)

(setopt menu-bar-mode nil)
(setopt tool-bar-mode nil)
(set-scroll-bar-mode nil)

;; `setopt' warns that Package.el is loaded before Elpaca.
(setq package-enable-at-startup nil)

(setopt use-package-hook-name-suffix nil)

(setopt use-package-always-ensure t)

(provide 'early-init)

;;; early-init.el ends here
