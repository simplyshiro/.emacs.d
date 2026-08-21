;;; init-mode-line.el --- Init Mode Line -*- lexical-binding: t; -*-

;; Author: simplyshiro
;; URL: https://github.com/simplyshiro/.emacs.d

;;; Commentary:

;;; Code:

(defvar-local shiro-mode-line-symbol-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] #'read-only-mode)
      map))
(defvar-local shiro-mode-line-symbol
    '(:eval (let* ((symbol (cond (buffer-read-only " %% ")
                                 ((buffer-modified-p) " * ")
                                 (t " - ")))
                   (status (cond (buffer-read-only "Buffer is read-only")
                                 ((buffer-modified-p) "Buffer is modified")
                                 (t "Buffer is unmodified")))
                   (help (format "%s
mouse-1: Toggle read-only mode" status)))
              (propertize symbol
                          'mouse-face 'mode-line-highlight
                          'help-echo help
                          'local-map shiro-mode-line-symbol-map))))
(put 'shiro-mode-line-symbol 'risky-local-variable t)

(defun shiro-mode-line-buffer-name-face ()
  "Return the appropriate face for `shiro-mode-line-buffer-name'."
  (let ((buffer-modified (buffer-modified-p))
        (window-selected (mode-line-window-selected-p)))
    (cond ((and buffer-modified window-selected)
           '(italic mode-line-buffer-id))
          (window-selected 'mode-line-buffer-id)
          (buffer-modified '(italic mode-line-inactive))
          (t 'mode-line-inactive))))
(defvar-local shiro-mode-line-buffer-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] #'consult-buffer)
      map))
(defvar-local shiro-mode-line-buffer-name
    '(:eval (let* ((name (buffer-name))
                   (file-name (buffer-file-name))
                   (path-string (if file-name (format "\npath: %s" file-name) ""))
                   (help (format "Buffer name%s
mouse-1: Switch buffer" path-string)))
              (propertize (format " %s " name)
                          'face (shiro-mode-line-buffer-name-face)
                          'mouse-face 'mode-line-highlight
                          'help-echo help
                          'local-map shiro-mode-line-buffer-map))))
(put 'shiro-mode-line-buffer-name 'risky-local-variable t)

(defvar-local shiro-mode-line-vc-branch-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] #'magit-status)
      (define-key map [mode-line mouse-3] #'magit-log-current)
      map))
(defvar-local shiro-mode-line-vc-branch
    '(:eval (when-let* ((file (buffer-file-name))
                        (branch (ignore-errors (vc-git--symbolic-ref file)))
                        (revision (ignore-errors (vc-working-revision file)))
                        (rev-short (if (> (length revision) 7)
                                       (substring revision 0 7)
                                     revision))
                        (help (format "Branch name\nrevision: %s
mouse-1: Show the status of the current Git repository
mouse-3: Show log for the current branch" rev-short)))
              (propertize (format " %s " branch)
                          'face 'italic
                          'mouse-face 'mode-line-highlight
                          'help-echo help
                          'local-map shiro-mode-line-vc-branch-map))))
(put 'shiro-mode-line-vc-branch 'risky-local-variable t)

(defvar-local shiro-mode-line-major-mode
    '(:eval (propertize (format " %s " (downcase (symbol-name major-mode)))
                        'mouse-face 'mode-line-highlight
                        'help-echo "Major mode")))
(put 'shiro-mode-line-major-mode 'risky-local-variable t)

(defvar-local shiro-mode-line-position
    '((line-number-mode
       (column-number-mode
        (" %l" (column-number-indicator-zero-based ":%C" ":%c"))
        " L%l")
       (column-number-mode
        (column-number-indicator-zero-based " C%C" " C%c")))
      " "))
(put 'shiro-mode-line-position 'risky-local-variable t)

(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '(" %e" shiro-mode-line-symbol
                shiro-mode-line-buffer-name
                shiro-mode-line-major-mode
                shiro-mode-line-vc-branch
                mode-line-format-right-align
                shiro-mode-line-position " "))

(provide 'init-mode-line)

;;; init-mode-line.el ends here
