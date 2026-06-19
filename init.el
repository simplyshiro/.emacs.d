;;; init.el --- Init -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(defgroup shiro nil
  "Customization variables for shiro's `init.el'."
  :tag "shiro"
  :group 'emacs
  :prefix "shiro-")

(add-to-list 'load-path (expand-file-name "lisp/" root-emacs-directory))

(require 'init-elpaca)
(require 'init-core)
(require 'init-ui)
(require 'init-dev)

(load custom-file 'noerror 'nomessage)

(provide 'init)

;;; init.el ends here
