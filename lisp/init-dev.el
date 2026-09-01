;;; init-dev.el --- Init Dev -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(add-hook 'prog-mode-hook #'electric-pair-local-mode)

;;; Language Server Protocol (LSP) Client

(with-eval-after-load 'eglot
  ;; Servers
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) "jdtls" "--enable-preview"))
  (add-to-list 'eglot-server-programs '(kotlin-ts-mode "kotlin-lsp" "--stdio"))
  (add-to-list 'eglot-server-programs '(python-base-mode "rass" "python"))
  (add-to-list 'eglot-server-programs '(qml-ts-mode "qmlls6"))
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode "rust-analyzer" :initializationOptions
                              (:check (:command "clippy"))))

  ;; Settings
  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)

  ;; Performance Optimizations
  (setq eglot-events-buffer-config '(:size 0 :format short))
  (setq eglot-report-progress nil)
  (setq eglot-sync-connect nil)

  ;; Invalidate cache on `eglot-completion-at-point'.
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; Automatically start `eglot' in specified modes.
(dolist (hook '(c-ts-mode-hook
                csharp-ts-mode-hook
                css-base-mode-hook
                html-ts-mode-hook
                java-ts-mode-hook
                js-base-mode-hook
                kotlin-ts-mode-hook
                lua-ts-mode-hook
                mhtml-ts-mode-hook
                python-base-mode-hook
                qml-ts-mode-hook
                rust-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

(use-package flymake
  :hook (prog-mode-hook . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :ensure nil)

(use-package treesit
  :config
  (setq major-mode-remap-alist
        '((c++-mode . c++-ts-mode)
          (c-mode . c-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          (html-mode . html-ts-mode)
          (java-mode . java-ts-mode)
          (js-mode . js-ts-mode)
          (mhtml-mode . html-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)))
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
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
                  (rust "v0.24.2")
                  (tsx "v0.23.2" "tree-sitter/tree-sitter-typescript" "tsx/src")
                  (typescript "v0.23.2" "tree-sitter/tree-sitter-typescript" "typescript/src")
                  ;; Unofficial `tree-sitter' Grammars
                  (kotlin "0.3.8" "fwcd/tree-sitter-kotlin")
                  (lua "v0.5.0" "tree-sitter-grammars/tree-sitter-lua")
                  (qmljs "0.3.0" "yuja/tree-sitter-qmljs"))))
  :preface
  (defun shiro--treesit-generate-language-source (lang revision &optional repo source-dir)
    (let ((url (format "https://github.com/%s"
                       (or repo (format "tree-sitter/tree-sitter-%s" lang)))))
      (delq nil (list lang url revision source-dir))))
  :mode ("\\.lua\\'" . lua-ts-mode)
  :ensure nil)

(use-package apheleia
  :config (apheleia-global-mode))

(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package qml-ts-mode
  :ensure (:host github :repo "xhcoding/qml-ts-mode"))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :ensure nil)

(provide 'init-dev)

;;; init-dev.el ends here
