;;; init-ui.el --- Init UI -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(use-package doric-themes
  :config (doric-themes-load-theme 'doric-light)
  :defer nil)

(use-package ef-themes
  :defer nil)

(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t))

(use-package standard-themes
  :defer nil)

(setopt mode-line-buffer-identification
        (propertize " %b " 'face 'mode-line-buffer-id
                    'mouse-face 'mode-line-highlight
                    'help-echo "Buffer name
mouse-1: Previous buffer
mouse-3: Next buffer"
                    'local-map mode-line-buffer-identification-keymap))
(setopt mode-line-position-line-format '(" L%l "))
(setopt mode-line-position-column-format '(" C%C "))
(setopt mode-line-position-column-line-format '(" %l:%C "))
(setopt mode-line-position
        '((line-number-mode
           (column-number-mode
            (:eval mode-line-position-column-line-format)
            (:eval mode-line-position-line-format))
           (column-number-mode
            (:eval mode-line-position-column-format)))))
(setopt mode-line-right-align-edge 'right-margin)
(setopt mode-line-format
        '("%e" " %% " (:eval mode-line-buffer-identification)
          (:eval (when-let (vc vc-mode)
                   (list " " (substring vc 5) " ")))
          (:eval
           (concat " ("
                   (downcase
                    (cond ((consp mode-name) (car mode-name))
                          ((stringp mode-name) mode-name)
                          (t "unknown")))
                   " mode) "))
          (flymake-mode flymake-mode-line-format)
          mode-line-position mode-line-format-right-align))

(defcustom shiro-font-height 105
  "Font height to use."
  :tag "shiro Font Height"
  :group 'shiro
  :type 'natnum)

(defun shiro-set-font-families (&optional frame)
  "Set font families on FRAME."
  (set-face-attribute 'default frame
                      :height shiro-font-height)
  (set-face-attribute 'fixed-pitch frame
                      :height shiro-font-height)
  (set-face-attribute 'variable-pitch frame
                      :height shiro-font-height))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'shiro-set-font-families)
  (shiro-set-font-families))

(use-package spacious-padding
  :init (spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:internal-border-width 16 :right-divider-width 1 :mode-line-width 8)))

(use-package display-line-numbers
  :hook ((eat-mode-hook ielm-mode-hook) . (lambda ()
                                            (display-line-numbers-mode -1)))
  :custom (display-line-numbers-type 'relative)
  :ensure nil)

(blink-cursor-mode -1)
(pixel-scroll-precision-mode)

(provide 'init-ui)

;;; init-ui.el ends here
