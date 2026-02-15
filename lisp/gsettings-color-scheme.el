;;; gsettings-color-scheme.el --- GSettings Color Scheme -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(defun gsettings-color-scheme--prefer-dark-p ()
  "Return non-nil if `color-scheme' is set to `prefer-dark'."
  (string-match-p "prefer-dark"
                  (shell-command-to-string
                   "gsettings get org.gnome.desktop.interface color-scheme")))

(defgroup gsettings-color-scheme nil
  "Sync Emacs theme with GSettings `color-scheme'."
  :group 'faces
  :group 'modus-themes
  :group 'ef-themes
  :prefix "gsettings-color-scheme-")

(defcustom gsettings-color-scheme-light-theme 'ef-trio-light
  "Theme to use when `color-scheme' is set to `prefer-light'."
  :type 'symbol
  :group 'gsettings-color-scheme)

(defcustom gsettings-color-scheme-dark-theme 'ef-trio-dark
  "Theme to use when `color-scheme' is set to `prefer-dark'."
  :type 'symbol
  :group 'gsettings-color-scheme)

(defvar gsettings-color-scheme--dbus-object nil
  "D-Bus object for GSettings `color-scheme'.")

(defun gsettings-color-scheme--handler (namespace key _)
  "Handle D-Bus signal for GSettings `color-scheme'.
Arguments are NAMESPACE and KEY."
  (when (and (string= namespace "org.gnome.desktop.interface")
             (string= key "color-scheme"))
    (gsettings-color-scheme-load-theme)))

(defun gsettings-color-scheme-load-theme ()
  "Load theme based on the current `color-scheme'."
  (let ((theme (if (gsettings-color-scheme--prefer-dark-p)
                   gsettings-color-scheme-dark-theme
                 gsettings-color-scheme-light-theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (cond
     ((and (fboundp 'modus-themes--modus-theme-p)
           (modus-themes--modus-theme-p theme))
      (if (fboundp 'modus-themes-load-theme)
          (modus-themes-load-theme theme)
        (load-theme theme :no-confirm))))))

(define-minor-mode gsettings-color-scheme-mode
  "Sync Emacs theme with GSettings `color-scheme'."
  :global t
  (require 'dbus)
  (if gsettings-color-scheme-mode
      (progn
        (setq gsettings-color-scheme--dbus-object
              (dbus-register-signal
               :session "org.freedesktop.portal.Desktop"
               "/org/freedesktop/portal/desktop"
               "org.freedesktop.portal.Settings"
               "SettingChanged"
               #'gsettings-color-scheme--handler))
        (gsettings-color-scheme-load-theme))
    (when gsettings-color-scheme--dbus-object
      (dbus-unregister-object gsettings-color-scheme--dbus-object)
      (setq gsettings-color-scheme--dbus-object nil))))

(provide 'gsettings-color-scheme)

;;; gsettings-color-scheme.el ends here
