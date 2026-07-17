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
               '((python-base-mode) "rass" "python"))
  (add-to-list 'eglot-server-programs '(qml-ts-mode "qmlls6"))
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) "rust-analyzer" :initializationOptions (:check (:command "clippy"))))
  (add-to-list 'completion-category-overrides
               '((eglot (styles orderless))
                 (eglot-capf (styles orderless))))
  :hook
  ((c-mode-hook c-ts-mode-hook) . eglot-ensure)
  ((csharp-mode-hook csharp-ts-mode-hook) . eglot-ensure)
  (css-base-mode-hook . eglot-ensure)
  ((html-mode-hook html-ts-mode-hook mhtml-mode-hook) . eglot-ensure)
  ((java-mode-hook java-ts-mode-hook) . eglot-ensure)
  (js-base-mode-hook . eglot-ensure)
  (kotlin-ts-mode-hook . eglot-ensure)
  (lua-ts-mode-hook . eglot-ensure)
  (python-base-mode-hook . eglot-ensure)
  (rust-ts-mode-hook . eglot-ensure)
  (qml-ts-mode-hook . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-extend-to-xref t)
  (eglot-report-progress nil)
  (eglot-sync-connect nil)
  :ensure nil)

(use-package flymake
  :hook (prog-mode-hook . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :ensure nil)

(use-package treesit
  :config
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
                  ;; Unofficial `tree-sitter' parsers
                  (kotlin "0.3.8" "fwcd/tree-sitter-kotlin")
                  (lua "v0.5.0" "tree-sitter-grammars/tree-sitter-lua")
                  (qmljs "0.3.0" "yuja/tree-sitter-qmljs"))))
  :preface
  (defun shiro--treesit-generate-language-source (lang revision &optional repo source-dir)
    (let ((url (format "https://github.com/%s"
                       (or repo (format "tree-sitter/tree-sitter-%s" lang)))))
      (delq nil (list lang url revision source-dir))))
  :mode ("\\.lua\\'" . lua-ts-mode)
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
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
  :ensure nil)

(use-package apheleia
  :config (apheleia-global-mode))

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

(use-package ghostel)

(provide 'init-dev)

;;; init-dev.el ends here
