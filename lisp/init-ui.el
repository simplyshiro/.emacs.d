;;; init-ui.el --- Init UI -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

;;; Fonts

(defun shiro--set-font (&optional frame)
  "Set fonts on FRAME."
  (let ((font-height 120))
    (set-face-attribute 'default frame :height font-height :weight 'medium)
    (set-face-attribute 'variable-pitch frame :height font-height :weight 'normal)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'shiro--set-font)
  (shiro--set-font))

;;; Padding

(use-package spacious-padding
  :init (spacious-padding-mode)
  :config (shiro--mode-line-update-right-align-edge)
  :preface (defun shiro--mode-line-update-right-align-edge ()
             "Update `right-align-edge' based on `spacious-padding-mode'."
             (setq mode-line-right-align-edge
                   (if spacious-padding-mode 'window 'right-margin)))
  :hook (spacious-padding-mode-hook . shiro--mode-line-update-right-align-edge)
  :custom (spacious-padding-widths
           '(:internal-border-width 16 :right-divider-width 1 :mode-line-width 8)))

;;; Line Numbers

(defun shiro--disable-display-line-numbers-mode ()
  "Disable `display-line-numbers-mode'."
  (display-line-numbers-mode -1))

(global-display-line-numbers-mode)

;; Disable line numbers on terminal and REPL buffers.
(dolist (hook '(eat-mode-hook
                ghostel-mode-hook
                ielm-mode-hook))
  (add-hook hook #'shiro--disable-display-line-numbers-mode))

;;; Miscellaneous

;; Only show the tab bar when there is more than one tab.
(setq tab-bar-show 1)

(blink-cursor-mode -1)

(defun shiro-pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

;; Pulse the current line after scrolling, recentering, or switching windows.
(dolist (command '(other-window
                   recenter-top-bottom
                   scroll-down-command
                   scroll-up-command))
  (advice-add command :after #'shiro-pulse-line))

(provide 'init-ui)

;;; init-ui.el ends here
