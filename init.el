;;; init.el --- Init -*- lexical-binding: t; no-byte-compile: t; no-native-compile: t; -*-

;;; Commentary:

;;; Code:

(defvar my-iosevka-ligatures '("<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->"
                               "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>"
                               ">=" ">>=" "<->" "<-->" "<--->" "<---->" "<=>"
                               "<==>" "<===>" "<====>" "::" ":::" "<~~" "</"
                               "</>" "/>" "~~>" "==" "!=" "<>" "===" "!=="
                               "!===" "<:" ":=" "*=" "*+" "<*" "<*>" "*>" "<|"
                               "<|>" "|>" "+*" "=*" "=:" ":>" "/*" "*/" "+++"
                               "<!--" "<!---")
  "List of ligatures in the default `calt' litigation set of Iosevka.")

;;; Elpaca

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

;;; Packages

;; Required by `ef-themes'.
(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-headings '((0 . (2.0))
                           (1 . (1.5))
                           (2 . (1.4))
                           (3 . (1.3))
                           (4 . (1.2))
                           (5 . (1.1))))
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  :config (modus-themes-load-theme 'modus-operandi))

;; Required by `magit'.
(use-package transient)

(use-package consult
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :bind (("C-x M-:" . consult-complex-command)
         ;; ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ;; ("C-c M-x" . consult-mode-command)
         ;; ("C-c m" . consult-man)
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
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :init (advice-add #'register-preview :override #'consult-register-window))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  :init (global-corfu-mode))

;; (use-package ef-themes
;;   :init
;;   (ef-themes-take-over-modus-themes-mode)
;;   (modus-themes-load-theme 'ef-trio-light))

(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode my-iosevka-ligatures)
  (global-ligature-mode))

(use-package magit)

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package meow
  :custom
  (meow-use-clipboard t)
  :init (defun meow-setup ()
          (setopt meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

          (meow-motion-define-key
           '("<escape>" . ignore)
           '("j" . meow-next)
           '("k" . meow-prev))

          (meow-leader-define-key
           '("1" . meow-digit-argument)
           '("2" . meow-digit-argument)
           '("3" . meow-digit-argument)
           '("4" . meow-digit-argument)
           '("5" . meow-digit-argument)
           '("6" . meow-digit-argument)
           '("7" . meow-digit-argument)
           '("8" . meow-digit-argument)
           '("9" . meow-digit-argument)
           '("0" . meow-digit-argument)
           '("b" . consult-buffer)
           '("/" . meow-keypad-describe-key)
           '("?" . meow-cheatsheet))

          (meow-normal-define-key
           '("<escape>" . ignore)
           '("1" . meow-expand-1)
           '("2" . meow-expand-2)
           '("3" . meow-expand-3)
           '("4" . meow-expand-4)
           '("5" . meow-expand-5)
           '("6" . meow-expand-6)
           '("7" . meow-expand-7)
           '("8" . meow-expand-8)
           '("9" . meow-expand-9)
           '("0" . meow-expand-0)
           '("-" . negative-argument)
           '("q" . meow-quit)
           '("Q" . meow-goto-line)
           '("w" . meow-mark-word)
           '("W" . meow-mark-symbol)
           '("e" . meow-next-word)
           '("E" . meow-next-symbol)
           '("r" . meow-replace)
           '("R" . meow-swap-grab)
           '("t" . meow-till)
           '("y" . meow-save)
           '("Y" . meow-sync-grab)
           '("u" . undo-only)
           '("U" . undo-redo)
           '("i" . meow-insert)
           '("I" . meow-open-above)
           '("o" . meow-block)
           '("O" . meow-to-block)
           '("p" . meow-yank)
           '("[" . meow-beginning-of-thing)
           '("]" . meow-end-of-thing)
           '("a" . meow-append)
           '("A" . meow-open-below)
           '("s" . meow-kill)
           '("d" . meow-delete)
           '("D" . meow-backward-delete)
           '("f" . meow-find)
           '("g" . meow-cancel-selection)
           '("G" . meow-grab)
           '("h" . meow-left)
           '("H" . meow-left-expand)
           '("j" . meow-next)
           '("J" . meow-next-expand)
           '("k" . meow-prev)
           '("K" . meow-prev-expand)
           '("l" . meow-right)
           '("L" . meow-right-expand)
           '(";" . meow-reverse)
           '("'" . repeat)
           '("z" . meow-pop-selection)
           '("x" . meow-line)
           '("X" . meow-goto-line)
           '("c" . meow-change)
           '("v" . meow-visit)
           '("b" . meow-back-word)
           '("B" . meow-back-symbol)
           '("n" . meow-search)
           '("m" . meow-join)
           '("," . meow-inner-of-thing)
           '("." . meow-bounds-of-thing)))
  :config
  (meow-setup)
  (meow-global-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package qml-ts-mode
  :ensure (:host github :repo "xhcoding/qml-ts-mode"))

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-scroll-margin (/ vertico-count 2))
  :init (vertico-mode))

;;; Mode Line

(setopt mode-line-buffer-identification
        `(:propertize
          " %b " face mode-line-buffer-id
          mouse-face mode-line-emphasis
          help-echo "Buffer name
mouse-1: Previous buffer
mouse-3: Next buffer"
          local-map ,mode-line-buffer-identification-keymap))
(setopt mode-line-position-line-format '(" L%l "))
(setopt mode-line-position-column-format '(" C%C "))
(setopt mode-line-position-column-line-format '(" %l:%C "))
(setopt mode-line-position
        '((line-number-mode
           (column-number-mode
            (:eval mode-line-position-column-line-format)
            (:eval mode-line-position-line-format))
           (column-number-mode
            (:eval mode-line-position-column-format)))))
(setopt mode-line-right-align-edge 'right-fringe)
(setopt mode-line-format
        '("%e"
          " λ "
          (:eval (with-eval-after-load 'meow (meow-indicator)))
          (:eval mode-line-buffer-identification)
          (:eval (concat
                  " (" (downcase
                        (cond ((consp mode-name) (car mode-name))
                              ((stringp mode-name) mode-name)
                              (t "unknown")))
                  " mode) "))
          mode-line-format-right-align
          mode-line-position))

;;; Tree-Sitter

(setopt treesit-language-source-alist
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

(add-to-list 'major-mode-remap-alist
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

;;; Eglot

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'java-mode-hook #'eglot-ensure)
(add-hook 'kotlin-ts-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(add-hook 'qml-ts-mode-hook #'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . ("jdtls" "--enable-preview")))
  (add-to-list 'eglot-server-programs
               '((kotlin-ts-mode) . ("kotlin-lsp" "--stdio")))
  ;; Ruff doesn't have autocompletion yet.
  ;; (add-to-list 'eglot-server-programs
  ;;              '((python-mode python-ts-mode) . ("ruff" "server")))
  (add-to-list 'eglot-server-programs
               '((qml-ts-mode) . ("qmlls6")))
  (add-hook 'after-save-hook #'eglot-format))

;;; Org

(setopt org-hide-emphasis-markers t)

(add-hook 'org-mode-hook #'variable-pitch-mode)

;;; Hooks

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'flymake-mode)

;;; Modes

(column-number-mode)
(context-menu-mode)
(line-number-mode)
(global-auto-revert-mode)
(global-display-line-numbers-mode)
(pixel-scroll-precision-mode)
(recentf-mode)
(savehist-mode)
(save-place-mode)
(which-key-mode)

(add-to-list 'auto-mode-alist
             '("\\.rs\\'" . rust-ts-mode))

;;; Settings

;; Move `auto-save-list' folder inside `user-emacs-directory'.
(setopt auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory))

(setopt backup-by-copying t)

;; Move backup files to a single folder.
(setopt backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; Delete excess backup files silently.
(setopt delete-old-versions t)

(setopt display-line-numbers-type 'relative)
(setopt enable-recursive-minibuffers t)

;; Use spaces for indentation.
(setopt indent-tabs-mode nil)

(setopt native-comp-async-query-on-exit t)

;; Hide commands which are specific to modes other than the current buffer's mode.
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; Try to indent first before doing completion.
(setopt tab-always-indent 'complete)

(setopt tab-width 4)

;; Maximum decoration level to be used by tree-sitter fontifications.
(setopt treesit-font-lock-level 4)

;; Set to 16 MiB, 32 MiB, and 64 MiB respectively.
(setopt undo-limit (my-convert-mebibytes-to-bytes 16))
(setopt undo-strong-limit (my-convert-mebibytes-to-bytes 32))
(setopt undo-outer-limit (my-convert-mebibytes-to-bytes 64))

;; Use `y' or `n' instead of `yes' or `no'.
(setopt use-short-answers t)

;; Make numbered backup files.
(setopt version-control t)

;;; modus-themes Customization

(defun my-modus-themes-custom-faces ()
  (modus-themes-with-colors
    (custom-set-faces
     `(mode-line-active ((,c :box (:line-width 8 :color ,bg-mode-line-active))))
     `(mode-line-inactive ((,c :box (:line-width 8 :color ,bg-mode-line-inactive)))))))

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

;;; Fonts

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (member "Iosevka" (font-family-list))
              (set-face-attribute 'default nil :family "Iosevka" :height 105)
              (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 105))
            (when (member "Inter" (font-family-list))
              (set-face-attribute 'variable-pitch nil :family "Inter" :height 105))))

(provide 'init)

;;; init.el ends here
