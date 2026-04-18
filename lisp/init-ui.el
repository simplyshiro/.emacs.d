;;; init-ui.el --- Init UI -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(use-package doric-themes
  :config (doric-themes-load-theme 'doric-light)
  :demand t)

(use-package ef-themes
  :demand t)

(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t))

(use-package standard-themes
  :demand t)

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

(defun shiro-set-font (&optional frame)
  "Set font families on FRAME."
  (let ((font-height 105))
    (set-face-attribute 'default frame :height font-height)
    (set-face-attribute 'fixed-pitch frame :height font-height)
    (set-face-attribute 'variable-pitch frame :height font-height)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'shiro-set-font)
  (shiro-set-font))

(use-package spacious-padding
  :init (spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:internal-border-width 16 :right-divider-width 1 :mode-line-width 8)))

(use-package display-line-numbers
  :preface (defun disable-display-line-numbers-mode ()
             "Disable `display-line-numbers-mode'."
             (display-line-numbers-mode -1))
  :hook ((eat-mode-hook ielm-mode-hook) . disable-display-line-numbers-mode)
  :custom (display-line-numbers-type 'relative)
  :ensure nil)

(blink-cursor-mode -1)
(pixel-scroll-precision-mode)

(provide 'init-ui)

;;; init-ui.el ends here
