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
    (set-face-attribute 'fixed-pitch frame :height font-height :weight 'medium)
    (set-face-attribute 'variable-pitch frame :height font-height :weight 'normal)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'shiro--set-font)
  (shiro--set-font))

;;; Padding

(use-package spacious-padding
  :init (spacious-padding-mode)
  :custom (spacious-padding-widths
           '(:internal-border-width 0
                                    :right-divider-width 1
                                    :fringe-width 8
                                    :tab-width 0
                                    :mode-line-width 8)))

;;; Line Numbers

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
