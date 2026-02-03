;;; init-core.el --- Init Core -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d
;;
;;; Commentary:
;;
;;; Code:

(use-package emacs
  :config
  (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
  (column-number-mode)
  (delete-selection-mode)
  (global-auto-revert-mode)
  (line-number-mode)
  (recentf-mode)
  (save-place-mode)
  (savehist-mode)
  (which-key-mode)
  (windmove-default-keybindings 'control)
  :hook (before-save-hook . delete-trailing-whitespace)
  :custom
  (backup-by-copying t)
  (delete-old-versions t)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  (tab-width 4)
  (text-mode-ispell-word-completion nil)
  (undo-limit (shiro-convert-from-mib-to-b 16))
  (undo-outer-limit (shiro-convert-from-mib-to-b 64))
  (undo-strong-limit (shiro-convert-from-mib-to-b 32))
  (use-short-answers t)
  (version-control t)
  :ensure nil)

(use-package vertico
  :config (vertico-mode)
  :defer nil
  :custom
  (vertico-scroll-margin (/ vertico-count 2))
  (vertico-resize t)
  (vertico-cycle t))

(use-package consult
  :config (advice-add #'register-preview :override #'consult-register-window)
  :bind (("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-c M-x" . consult-mode-command)
         ("M-y" . consult-yank-pop)
         ("C-x r b" . consult-bookmark)
         ("C-x M-:" . consult-complex-command)
         ("C-c h" . consult-history)
         ("M-s e" . consult-isearch-history)
         ("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("M-s r" . consult-grep-match)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("C-c m" . consult-man)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-c k" . consult-kmacro)
         ("C-M-#" . consult-register)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         :map isearch-mode-map
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :custom
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package corfu
  :config (corfu-popupinfo-mode)
  :hook (prog-mode-hook . corfu-mode)
  :custom
  (corfu-scroll-margin (/ corfu-count 2))
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-popupinfo-delay '(0.01 . 1.0)))

(use-package marginalia
  :config (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :defer nil)

(use-package embark
  :config (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil (window-parameters (mode-line-format . none))))
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'init-core)

;;; init-core.el ends here
