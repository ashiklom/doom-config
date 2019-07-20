;;; lang/my-ess/autoload/drake.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ans/ess-drake-readd ()
  "Run `drake::readd` on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "drake::readd('%s')"))

;;;###autoload
(defun ans/ess-drake-loadd ()
  "Run `drake::loadd` on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "drake::loadd('%s')"))

;;;###autoload
(defun ans/ess-drake-build-target ()
  "Build drake target symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "invisible(drake::r_drake_build('%1$s')); drake::loadd('%1$s')"))
