;;; init-completion.el --- Init Completion -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t)
  (setq vertico-scroll-margin (/ vertico-count 2)))

(use-package corfu
  :init (global-corfu-mode)
  :config
  (setq corfu-cycle t)
  (setq corfu-preselect 'prompt)
  (setq corfu-scroll-margin (/ corfu-count 2))
  (setq corfu-quit-no-match t)

  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay 0.001))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  :preface (defun shiro--setup-elisp-capf ()
             "Set `cape-elisp-symbol' as the sole entry in `completion-at-point-functions'."
             (setq-local completion-at-point-functions '(cape-elisp-symbol)))
  :hook ((emacs-lisp-mode-hook ielm-mode-hook) . shiro--setup-elisp-capf)
  :bind ("C-c p" . cape-prefix-map))

(use-package orderless
  :init (setq completion-styles '(orderless basic))
  :config
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((eglot-capf (styles orderless))
                                        (file (styles partial-completion))))
  (setq completion-pcm-leading-wildcard t))

(provide 'init-completion)

;;; init-completion.el ends here
