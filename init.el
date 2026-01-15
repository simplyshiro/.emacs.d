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

;;; Built-in:

(use-package emacs
  :custom
  (blink-cursor-mode nil)
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

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) "jdtls" "--enable-preview"))
  (add-to-list 'eglot-server-programs
               '(kotlin-ts-mode "kotlin-lsp" "--stdio"))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) "ty" "server"))
  (add-to-list 'eglot-server-programs
               '(qml-ts-mode "qmlls6"))
  (setopt completion-category-defaults nil)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  ((c-mode-hook c-ts-mode-hook) . eglot-ensure)
  ((csharp-mode-hook csharp-ts-mode-hook) . eglot-ensure)
  ((java-mode-hook java-ts-mode-hook) . eglot-ensure)
  (kotlin-ts-mode-hook . eglot-ensure)
  ((python-mode-hook python-ts-mode-hook) . eglot-ensure)
  (rust-ts-mode-hook . eglot-ensure)
  (qml-ts-mode-hook . eglot-ensure)
  :custom
  (eglot-events-buffer-config '(:size 0))
  :ensure nil)

(use-package elec-pair
  :hook (prog-mode-hook . electric-pair-mode)
  :ensure nil)

(use-package files
  :custom
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)
  :ensure nil)

(use-package flymake
  :hook (prog-mode-hook . flymake-mode)
  :ensure nil)

(use-package org
  :hook (org-mode-hook . variable-pitch-mode)
  :custom (org-hide-emphasis-markers t)
  :ensure nil)

(use-package recentf
  :init (recentf-mode)
  :ensure nil)

(use-package rust-ts-mode
  :mode "\\.rs\\'"
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

(use-package treesit
  :init (setopt treesit-language-source-alist
                '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.25.0"))
                  (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.24.1"))
                  (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp" "v0.23.1"))
                  (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))
                  (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.25.0"))
                  (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.25.0"))
                  (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
                  (java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.23.5"))
                  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.25.0"))
                  (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
                  (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin" "0.3.8"))
                  (php . ("https://github.com/tree-sitter/tree-sitter-php" "v0.24.2" "php/src"))
                  (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.25.0"))
                  (qmljs . ("https://github.com/yuja/tree-sitter-qmljs" "0.3.0"))
                  (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1"))
                  (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.24.0"))
                  (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
                  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))))
  :custom
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode)
     (c++-mode . c++-ts-mode)
     (csharp-mode . csharp-ts-mode)
     (css-mode . css-ts-mode)
     (html-mode . html-ts-mode)
     (java-mode . java-ts-mode)
     (js-mode . js-ts-mode)
     (python-mode . python-ts-mode)
     (ruby-mode . ruby-ts-mode)))
  (treesit-font-lock-level 4)
  :ensure nil)

(use-package which-key
  :init (which-key-mode)
  :ensure nil)

;;; Packages:

;; Required by `magit'.
(use-package transient)

(use-package apheleia
  :init (apheleia-global-mode))

(use-package cape
  :hook (emacs-lisp-mode-hook . (lambda ()
                                  (setq-local completion-at-point-functions
                                              (list (cape-capf-super
                                                     #'cape-elisp-symbol
                                                     #'cape-dabbrev
                                                     #'cape-file)))))
  :bind ("C-c p" . cape-prefix-map))

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

(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

(use-package magit)

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((eglot (styles orderless))
                                   (eglot-capf (styles orderless))
                                   (file (styles basic partial-completion)))))

(use-package qml-ts-mode
  :ensure (:host github :repo "xhcoding/qml-ts-mode"))

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-scroll-margin (/ vertico-count 2)))

(provide 'init)

;;; init.el ends here
