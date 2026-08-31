;;; init.el --- Init -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/" root-emacs-directory))

(require 'init-elpaca)
(require 'init-core)
(require 'init-mode-line)
(require 'init-themes)
(require 'init-ui)
(require 'init-completion)
(require 'init-dev)
(require 'init-terminal)

(load custom-file 'noerror 'nomessage)

(provide 'init)

;;; init.el ends here
