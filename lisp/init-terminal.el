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
    :preface (defun ghostel-other-window (&optional arg)
               "Like `ghostel', but displayed in another window."
               (interactive "P")
               (let ((display-buffer-overriding-action
                      '((display-buffer-pop-up-window display-buffer-use-some-window)
                        (inhibit-same-window . t)))
                     (switch-to-buffer-obey-display-actions t))
                 (ghostel arg)))
    :commands (ghostel-download-module)
    :bind (("C-c t" . ghostel)
           ("C-x 4 t" . ghostel-other-window))))

(provide 'init-terminal)

;;; init-terminal.el ends here
