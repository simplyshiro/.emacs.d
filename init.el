;;; init.el --- Init -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d
;;
;;; Commentary:
;;
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'init-elpaca)
(require 'init-core)
(require 'init-ui)
(require 'init-dev)

(provide 'init)

;;; init.el ends here
