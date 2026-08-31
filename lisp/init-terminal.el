;;; init-terminal.el --- Init Terminal -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

;; Use `eat' on Termux. Otherwise, use `ghostel'. Bound to "C-c t" in both
;; environments.
(if (getenv "TERMUX_VERSION")
    (use-package eat
      :config (setq eat-kill-buffer-on-exit t)
      :bind ("C-c t" . eat))
  (use-package ghostel
    :config (setq ghostel-shell-integration nil)
    :commands (ghostel-download-module)
    :bind ("C-c t" . ghostel)))

(provide 'init-terminal)

;;; init-terminal.el ends here
