;;; ~/.doom.d/lisp/python.el -*- lexical-binding: t; -*-

(defun ans/python-print-symbol-at-point ()
  "Print symbol at point"
  (interactive)
  (let* ((sym (thing-at-point 'symbol t))
         (cmd (format "print(%s)" sym)))
    (python-shell-send-string cmd)))
