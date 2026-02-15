;;; init-dev.el --- Init Dev -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(add-hook 'prog-mode-hook #'electric-pair-mode)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) "jdtls" "--enable-preview"))
  (add-to-list 'eglot-server-programs '(kotlin-ts-mode "kotlin-lsp" "--stdio"))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) "rass" "python"))
  (add-to-list 'eglot-server-programs '(qml-ts-mode "qmlls6"))

  ;; Use `orderless' filtering for `eglot' completions
  (add-to-list 'completion-category-overrides
               '((eglot (styles orderless))
                 (eglot-capf (styles orderless))))
  :hook
  ((c-mode-hook c-ts-mode-hook) . eglot-ensure)
  ((csharp-mode-hook csharp-ts-mode-hook) . eglot-ensure)
  ((css-mode-hook css-ts-mode-hook) . eglot-ensure)
  ((html-mode-hook html-ts-mode-hook) . eglot-ensure)
  ((java-mode-hook java-ts-mode-hook) . eglot-ensure)
  ((js-mode-hook js-ts-mode-hook) . eglot-ensure)
  (kotlin-ts-mode-hook . eglot-ensure)
  ((python-mode-hook python-ts-mode-hook) . eglot-ensure)
  (rust-ts-mode-hook . eglot-ensure)
  (qml-ts-mode-hook . eglot-ensure)
  :ensure nil)

(use-package flymake
  :hook (prog-mode-hook . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :custom (flymake-mode-line-format
           '(" " flymake-mode-line-counters " "))
  :ensure nil)

(use-package treesit
  :config
  (setopt treesit-language-source-alist
          (mapcar (lambda (args) (apply #'shiro--treesit-generate-language-source args))
                  '((bash "v0.25.1")
                    (c "v0.24.1")
                    (c-sharp "v0.23.1")
                    (cpp "v0.23.4")
                    (css "v0.25.0")
                    (go "v0.25.0")
                    (html "v0.23.2")
                    (java "v0.23.5")
                    (javascript "v0.25.0")
                    (json "v0.24.8")
                    (php "v0.24.2" nil "php/src")
                    (python "v0.25.0")
                    (ruby "v0.23.1")
                    (rust "v0.24.0")
                    (tsx "v0.23.2" "tree-sitter/tree-sitter-typescript" "tsx/src")
                    (typescript "v0.23.2" "tree-sitter/tree-sitter-typescript" "typescript/src")
                    ;; Unofficial `tree-sitter' parsers
                    (kotlin "0.3.8" "fwcd/tree-sitter-kotlin")
                    (qmljs "0.3.0" "yuja/tree-sitter-qmljs"))))
  :preface
  (defun shiro--treesit-generate-language-source (lang revision &optional repo source-dir)
    "Generate a list to use with `treesit-language-source-alist'.
LANG is the language symbol.  REPO is the GitHub repository.  REVISION is the
Git tag or branch of the desired version.  SOURCE-DIR is the relative directory
in the repository in which the grammar's parser.c file resides."
    (list lang
          (concat "https://github.com/"
                  (or repo (concat "tree-sitter/tree-sitter-"
                                   (symbol-name lang))))
          revision
          source-dir))
  :custom
  (treesit-font-lock-level 4)
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
  :ensure nil)

(use-package apheleia
  :hook (prog-mode-hook . apheleia-mode))

;; `magit' requires the latest version of `transient'
(use-package transient)

(use-package magit)

(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package qml-ts-mode
  :ensure (:host github :repo "xhcoding/qml-ts-mode"))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :ensure nil)

(use-package eat
  :hook (eat-mode-hook . (lambda ()
                           (display-line-numbers-mode -1))))

(use-package ielm
  :preface
  (defun shiro--setup-ielm-mode ()
    "Setup `inferior-emacs-lisp-mode'."
    (setq-local comint-input-ring-file-name
                (expand-file-name "ielm-history.eld" shiro-variable-directory))
    (display-line-numbers-mode -1))
  :hook (ielm-mode-hook . shiro--setup-ielm-mode)
  :ensure nil)

(provide 'init-dev)

;;; init-dev.el ends here
