;;; early-init.el --- Early Init -*- lexical-binding: t; no-byte-compile: t; no-native-compile: t; -*-

;;; Commentary:

;;; Code:

(defun shiro-convert-from-mebibytes-to-bytes (mebibytes)
  (* mebibytes (expt 2 20)))

(defconst shiro-gc-cons-threshold (shiro-convert-from-mebibytes-to-bytes 64))

(defun shiro-setup-startup-hooks ()
  (add-hook 'before-init-hook
            (lambda ()
              (setopt gc-cons-threshold most-positive-fixnum)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setopt gc-cons-threshold shiro-gc-cons-threshold))))

(shiro-setup-startup-hooks)

(setq package-enable-at-startup nil)

(setopt load-prefer-newer t)

(setopt package-native-compile t)

(setopt user-emacs-directory (expand-file-name "var/" user-emacs-directory))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(when (eq system-type 'windows-nt)
  (elpaca-no-symlink-mode))
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setopt initial-buffer-choice nil)
(setopt inhibit-startup-screen t)
(setopt inhibit-startup-echo-area-message user-login-name)
(setopt inhibit-startup-buffer-menu t)
(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message nil)

(setopt menu-bar-mode nil)
(setopt scroll-bar-mode nil)
(setopt tool-bar-mode nil)

(setopt use-package-hook-name-suffix nil)
(setopt use-package-always-ensure t)

(setopt auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory))

(provide 'early-init)

;;; early-init.el ends here
