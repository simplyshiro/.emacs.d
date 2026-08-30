;;; init-ui.el --- Init UI -*- lexical-binding: t; -*-

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

(defun shiro-set-font (&optional frame)
  "Set font families on FRAME."
  (let ((font-height 120))
    (set-face-attribute 'default frame :height font-height :weight 'medium)
    (set-face-attribute 'variable-pitch frame :height font-height :weight 'normal)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'shiro-set-font)
  (shiro-set-font))

(use-package spacious-padding
  :init (spacious-padding-mode)
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 16 :right-divider-width 1 :mode-line-width 8))
  (shiro--mode-line-update-right-align-edge)
  :preface (defun shiro--mode-line-update-right-align-edge ()
             "Update `right-align-edge' based on `spacious-padding-mode'."
             (setq mode-line-right-align-edge
                   (if spacious-padding-mode 'window 'right-margin)))
  :hook (spacious-padding-mode-hook . shiro--mode-line-update-right-align-edge))

(use-package display-line-numbers
  :preface (defun shiro--disable-display-line-numbers-mode ()
             "Disable `display-line-numbers-mode'."
             (display-line-numbers-mode -1))
  :hook ((eat-mode-hook ielm-mode-hook) . shiro--disable-display-line-numbers-mode)
  :ensure nil)

(blink-cursor-mode -1)

(setq tab-bar-show 1)

(defun shiro-pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (command '(other-window
                   recenter-top-bottom
                   scroll-down-command
                   scroll-up-command))
  (advice-add command :after #'shiro-pulse-line))

(provide 'init-ui)

;;; init-ui.el ends here
