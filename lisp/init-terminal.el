;;; init-terminal.el --- Init Terminal -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(use-package ghostel
  :config
  (setq ghostel-module-auto-install 'download)
  (setq ghostel-shell-integration nil)
  :preface (defun ghostel-other-window (&optional arg)
             "Like `ghostel', but displayed in another window."
             (interactive "P")
             (let ((display-buffer-overriding-action
                    '((display-buffer-pop-up-window display-buffer-use-some-window)
                      (inhibit-same-window . t))))
               (ghostel arg)))
  :bind (("C-c t" . ghostel)
         ("C-x 4 t" . ghostel-other-window)))

(provide 'init-terminal)

;;; init-terminal.el ends here
