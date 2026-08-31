;;; init-core.el --- Init Core -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(defgroup shiro nil
  "Customization variables for shiro's `init.el'."
  :tag "shiro"
  :group 'emacs
  :prefix "shiro-")

(defun shiro-convert-to-bytes (value unit)
  "Convert VALUE in UNIT to bytes.
UNIT must be a keyword symbol like `:kib', `:mib', or `:gib'."
  (let ((multiplier (pcase unit
                      (:kib 1024)
                      (:mib 1048576)
                      (:gib 1073741824)
                      (_ (error "Unsupported unit: %s" unit)))))
    (* value multiplier)))

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
  (windmove-default-keybindings 'meta)
  (setq backup-by-copying t)
  (setq backup-by-copying-when-linked t)
  (setq create-lockfiles nil)
  (setq delete-old-versions t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq enable-recursive-minibuffers t)
  (setq-default indent-tabs-mode nil)
  (setq make-backup-files nil)
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq remote-file-name-inhibit-locks t)
  (setq tab-always-indent 'complete)
  (setq-default tab-width 4)
  (setq text-mode-ispell-word-completion nil)
  (setq tramp-use-scp-direct-remote-copying t)
  (setq undo-limit (shiro-convert-to-bytes 16 :mib))
  (setq undo-outer-limit (shiro-convert-to-bytes 64 :mib))
  (setq undo-strong-limit (shiro-convert-to-bytes 32 :mib))
  (setq use-short-answers t)
  (setq version-control t)
  (setq view-read-only t)
  :hook (before-save-hook . delete-trailing-whitespace)
  :bind (([remap capitalize-word] . capitalize-dwim)
         ([remap downcase-word] . downcase-dwim)
         ([remap upcase-word] . upcase-dwim))
  :ensure nil)

(use-package compat)

(use-package consult
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  (setq consult-narrow-key "<")
  (setq xref-show-definitions-function #'consult-xref)
  (setq xref-show-xrefs-function #'consult-xref)
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
         ("M-r" . consult-history)))

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package embark
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil (window-parameters (mode-line-format . none))))
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(provide 'init-core)

;;; init-core.el ends here
