;;; init-ui.el --- Init UI -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d
;;
;;; Commentary:
;;
;;; Code:

(use-package modus-themes
  :config (modus-themes-include-derivatives-mode)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-common-palette-overrides
   '((primary90 "#eaddff")
     (primary80 "#d0bcff")
     (primary40 "#6750a4")
     (primary30 "#4f378b")
     (neutral98 "#fef7ff")
     (neutral90 "#e6e0e9")
     (neutral10 "#1d1b20")
     (neutral6 "#141218")
     (neutral-variant90 "#e7e0ec")
     (neutral-variant80 "#cac4d0")
     (neutral-variant30 "#49454f")
     (fringe unspecified)
     (bg-line-number-inactive unspecified)
     (bg-line-number-active unspecified)))
  (modus-themes-headings
   '((0 . (2.0))
     (1 . (1.5))
     (2 . (1.4))
     (3 . (1.3))
     (4 . (1.2))
     (5 . (1.1))))
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t))

(use-package ef-themes
  :config (modus-themes-load-theme 'ef-trio-light)
  :defer nil
  :custom
  (ef-trio-dark-palette-overrides
   '((primary primary80)
     (primary-container primary30)
     (on-primary-container primary90)
     (surface neutral6)
     (on-surface neutral90)
     (surface-variant neutral-variant30)
     (on-surface-variant neutral-variant80)
     (cursor primary)
     (bg-main surface)
     (bg-alt surface-variant)
     (fg-main on-surface)
     (fg-dim on-surface-variant)
     (bg-mode-line-active primary-container)
     (fg-mode-line-active on-primary-container)
     (bg-region primary-container)
     (fg-region on-primary-container)))
  (ef-trio-light-palette-overrides
   '((primary primary40)
     (primary-container primary90)
     (on-primary-container primary30)
     (surface neutral98)
     (on-surface neutral10)
     (surface-variant neutral-variant90)
     (on-surface-variant neutral-variant30)
     (cursor primary)
     (bg-main surface)
     (bg-alt surface-variant)
     (fg-main on-surface)
     (fg-dim on-surface-variant)
     (bg-mode-line-active primary-container)
     (fg-mode-line-active on-primary-container)
     (bg-region primary-container)
     (fg-region on-primary-container))))

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
          flymake-mode-line-format
          mode-line-position mode-line-format-right-align))

(defvar-local shiro--font-height 105)
(defvar-local shiro--fixed-pitch-font-family "Google Sans Code")
(defvar-local shiro--variable-pitch-font-family "Google Sans")

(defun shiro--set-font-families (&optional frame)
  "Set preferred font families on FRAME."
  (when (member shiro--fixed-pitch-font-family (font-family-list))
    (set-face-attribute 'default frame
                        :family shiro--fixed-pitch-font-family
                        :height shiro--font-height)
    (set-face-attribute 'fixed-pitch frame
                        :family shiro--fixed-pitch-font-family
                        :height shiro--font-height))
  (when (member shiro--variable-pitch-font-family (font-family-list))
    (set-face-attribute 'variable-pitch frame
                        :family shiro--variable-pitch-font-family
                        :height shiro--font-height)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'shiro--set-font-families)
  (shiro--set-font-families))

(use-package spacious-padding
  :config (spacious-padding-mode)
  :defer nil
  :custom
  (spacious-padding-widths
   '(:internal-border-width 16 :right-divider-width 1 :mode-line-width 8)))

(use-package display-line-numbers
  :hook (prog-mode-hook . display-line-numbers-mode)
  :custom (display-line-numbers-type 'relative)
  :ensure nil)

(blink-cursor-mode -1)
(pixel-scroll-precision-mode)

(provide 'init-ui)

;;; init-ui.el ends here
