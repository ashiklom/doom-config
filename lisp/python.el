;;; ~/.doom.d/lisp/python.el -*- lexical-binding: t; -*-

;; (add-hook! anaconda-eldoc-mode :append #'ans/python-tramp-config)

;; (defun ans/python-tramp-config ()
;;   "Python configuration for Tramp."
;;   (if (file-remote-p default-directory)
;;       (progn
;;         ;; Disable eldoc mode (doesn't work well with tramp)
;;         (message "TRAMP mode detected. Disabling eldoc and autocomplete.")
;;         (setq-local eldoc-mode nil)
;;         ;; Turn off autocompletion
;;         (setq-local company-idle-delay nil))))

(add-hook! python-mode #'turn-off-smartparens-mode #'electric-pair-local-mode)

(defun ans/python-print-symbol-at-point ()
  "Print symbol at point"
  (interactive)
  (let* ((sym (thing-at-point 'symbol t))
         (cmd (format "print(%s)" sym)))
    (python-shell-send-string cmd)))

(defun ans/python-send-paragraph ()
  "Send paragraph"
  (interactive)
  (save-excursion
    (mark-paragraph)
    (python-shell-send-region (point) (mark))
    (deactivate-mark)))
