;;; init-themes.el --- Init Themes -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(defcustom shiro-theme 'doric-light
  "Theme to load on startup."
  :tag "shiro Theme"
  :type 'symbol
  :group 'shiro)

(defun shiro--load-theme ()
  "Load `shiro-theme'."
  (unless (custom-theme-enabled-p shiro-theme)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme shiro-theme :no-confirm)))

(add-hook 'elpaca-after-init-hook #'shiro--load-theme)

(use-package doric-themes
  :demand t)

(use-package ef-themes
  :demand t)

(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-mixed-fonts t))

(use-package standard-themes
  :demand t)

(provide 'init-themes)

;;; init-themes.el ends here
