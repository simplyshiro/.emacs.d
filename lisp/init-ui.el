;;; init-ui.el --- Init UI -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

;;; Fonts

(defcustom shiro-fixed-pitch-font-family 'nil
  "Fixed pitch font family to use."
  :tag "shiro Fixed Pitch Font Family"
  :group 'shiro
  :type '(choice (const :tag "Default" nil)
                 (cons (string :tag "Font Family")
                       (symbol :tag "Weight"))))

(defcustom shiro-variable-pitch-font-family 'nil
  "Variable pitch font family to use."
  :tag "shiro Variable Pitch Font Family"
  :group 'shiro
  :type '(choice (const :tag "Default" nil)
                 (cons (string :tag "Font Family")
                       (symbol :tag "Weight"))))

(defun shiro--set-font (&optional frame)
  "Set fonts on FRAME."
  (let ((font-height 120)
        (fixed-pitch-weight (or (cdr shiro-fixed-pitch-font-family) 'normal))
        (variable-pitch-weight (or (cdr shiro-variable-pitch-font-family)
                                   'normal)))
    (set-face-attribute 'default frame
                        :family (car shiro-fixed-pitch-font-family)
                        :height font-height
                        :weight fixed-pitch-weight)
    (set-face-attribute 'fixed-pitch frame
                        :family (car shiro-fixed-pitch-font-family)
                        :height font-height
                        :weight fixed-pitch-weight)
    (set-face-attribute 'variable-pitch frame
                        :family (car shiro-variable-pitch-font-family)
                        :height font-height
                        :weight variable-pitch-weight)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'shiro--set-font)
  (add-hook 'after-init-hook #'shiro--set-font))

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

;; URL: https://karthinks.com/software/batteries-included-with-emacs/
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
