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
  (setq corfu-popupinfo-delay '(0.01 . 0.004)))

(use-package orderless
  :init (setq completion-styles '(orderless basic))
  :config
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))))
  (setq completion-pcm-leading-wildcard t))

(provide 'init-completion)

;;; init-completion.el ends here
