;;; init.el --- Init -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d
;;
;;; Commentary:
;;
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'init-elpaca)
(require 'init-ui)
(require 'init-dev)

;;; Built-in:

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (tab-width 4)
  (tab-always-indent 'complete)
  (use-short-answers t)
  (undo-limit (shiro-convert-from-mib-to-b 16))
  (undo-strong-limit (shiro-convert-from-mib-to-b 32))
  (undo-outer-limit (shiro-convert-from-mib-to-b 64))
  :config
  (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
  :ensure nil)

(use-package autorevert
  :init (global-auto-revert-mode)
  :ensure nil)

(use-package comp-run
  :custom (native-comp-async-query-on-exit t)
  :ensure nil)

(use-package files
  :custom
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)
  :ensure nil)

(use-package org
  :hook (org-mode-hook . variable-pitch-mode)
  :custom (org-hide-emphasis-markers t)
  :ensure nil)

(use-package recentf
  :init (recentf-mode)
  :ensure nil)

(use-package savehist
  :init (savehist-mode)
  :ensure nil)

(use-package saveplace
  :init (save-place-mode)
  :ensure nil)

(use-package simple
  :init
  (column-number-mode)
  (line-number-mode)
  :hook (before-save-hook . delete-trailing-whitespace)
  :custom
  (indent-tabs-mode nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :ensure nil)

(use-package text-mode
  :custom
  (text-mode-ispell-word-completion nil)
  :ensure nil)

(use-package which-key
  :init (which-key-mode)
  :ensure nil)

;;; Packages:

(use-package consult
  :init (advice-add #'register-preview :override #'consult-register-window)
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :bind (("C-x M-:" . consult-complex-command)
         ([remap Info-search] . consult-info)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-g e" . consult-compile-error)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g o" . consult-outline)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g k" . consult-global-mark)
         ("M-g m" . consult-mark)
         ("M-s e" . consult-isearch-history)
         ("M-s r" . consult-ripgrep)
         ("M-s u" . consult-focus-lines)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s k" . consult-keep-lines)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s c" . consult-locate)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-r" . consult-history)
         ("M-s" . consult-history))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :hook
  (eshell-mode-hook . (lambda ()
                        (setq-local corfu-map
                                    (define-keymap
                                      :parent corfu-map
                                      "RET" #'corfu-send))
                        (setq-local corfu-auto nil)))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)
  (corfu-popupinfo-delay '(0.1 . 1.0)))

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-scroll-margin (/ vertico-count 2)))

(provide 'init)

;;; init.el ends here
