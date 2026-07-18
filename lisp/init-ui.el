;;; init-ui.el --- Init UI -*- lexical-binding: t; no-byte-compile: t; -*-

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
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t))

(use-package standard-themes
  :demand t)

(defvar-local shiro-mode-line-symbol
    '(:eval (cond (buffer-read-only " %% ")
                  ((buffer-modified-p) " * ")
                  (t " - "))))
(put 'shiro-mode-line-symbol 'risky-local-variable t)

(defun shiro-mode-line-buffer-name-face ()
  "Return the appropriate face for `shiro-mode-line-buffer-name'."
  (let ((buffer-modified (buffer-modified-p))
        (window-selected (mode-line-window-selected-p)))
    (cond ((and buffer-modified window-selected)
           '(italic mode-line-buffer-id))
          (window-selected 'mode-line-buffer-id)
          (buffer-modified '(italic mode-line-inactive))
          (t 'mode-line-inactive))))
(defvar-local shiro-mode-line-buffer-name
    '(:eval (let* ((name (buffer-name))
                   (file-name (buffer-file-name))
                   (help (concat "Buffer name"
                                 (when file-name
                                   (concat "\npath: " file-name)))))
              (propertize (format " %s " name)
                          'face (shiro-mode-line-buffer-name-face)
                          'mouse-face 'mode-line-highlight
                          'help-echo help))))
(put 'shiro-mode-line-buffer-name 'risky-local-variable t)

(defvar-local shiro-mode-line-vc-branch
    '(:eval (when-let* ((file (buffer-file-name))
                        (branch (ignore-errors (vc-git--symbolic-ref file)))
                        (revision (ignore-errors (vc-working-revision file)))
                        (rev-short (if (> (length revision) 7)
                                       (substring revision 0 7)
                                     revision))
                        (help (concat "Branch name\nrevision: " rev-short)))
              (propertize (format " %s " branch)
                          'face 'italic
                          'mouse-face 'mode-line-highlight
                          'help-echo help))))
(put 'shiro-mode-line-vc-branch 'risky-local-variable t)

(defvar-local shiro-mode-line-major-mode
    '(:eval (propertize (format " %s " (downcase (symbol-name major-mode)))
                        'mouse-face 'mode-line-highlight
                        'help-echo "Major mode")))
(put 'shiro-mode-line-major-mode 'risky-local-variable t)

(defvar-local shiro-mode-line-position
    '((line-number-mode
       (column-number-mode
        (" %l" (column-number-indicator-zero-based ":%C" ":%c"))
        " L%l")
       (column-number-mode
        (column-number-indicator-zero-based " C%C" " C%c")))
      " "))
(put 'shiro-mode-line-position 'risky-local-variable t)

(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e" shiro-mode-line-symbol
                shiro-mode-line-buffer-name
                shiro-mode-line-major-mode
                shiro-mode-line-vc-branch
                mode-line-format-right-align
                shiro-mode-line-position))

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
  :config (shiro--mode-line-update-right-align-edge)
  :preface (defun shiro--mode-line-update-right-align-edge ()
             "Update `right-align-edge' based on `spacious-padding-mode'."
             (setq mode-line-right-align-edge
                   (if spacious-padding-mode 'window 'right-margin)))
  :hook (spacious-padding-mode-hook . shiro--mode-line-update-right-align-edge)
  :custom (spacious-padding-widths
           '(:internal-border-width 16 :right-divider-width 1 :mode-line-width 8)))

(use-package display-line-numbers
  :preface (defun shiro--disable-display-line-numbers-mode ()
             "Disable `display-line-numbers-mode'."
             (display-line-numbers-mode -1))
  :hook ((eat-mode-hook ielm-mode-hook) . shiro--disable-display-line-numbers-mode)
  :ensure nil)

(blink-cursor-mode -1)
(pixel-scroll-precision-mode)

(setq tab-bar-show 1)

;; https://karthinks.com/software/batteries-included-with-emacs/
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
