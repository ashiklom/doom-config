;;; ~/.doom.d/lisp/iterm.el -*- lexical-binding: t; -*-

;; Send code to a running Iterm instance
;; Adapted from https://github.com/haberdashPI/iterm.el

(defun iterm-last-char-p (str char)
  (let ((length (length str)))
    (and (> length 0)
         (char-equal (elt str (- length 1)) char))))

(defun iterm-chop-newline (str)
  (let ((length (length str)))
    (if (iterm-last-char-p str ?\n)
        (substring str 0 (- length 1))
      str)))

(defun iterm-maybe-add-newline (str)
  (if (not (iterm-last-char-p str ?\n))
      (concat str "\n")
    str))

(defun iterm-handle-newline (str)
  (iterm-maybe-add-newline (iterm-chop-newline str)))

(defun iterm-send-string (str)
  "Send STR to a running iTerm instance."
  (let* ((tf (make-temp-file "iterm-"))
         (str (iterm-handle-newline str))
         (default-directory (if (file-remote-p default-directory)
                                "~" default_directory)))
    (with-temp-file tf
      (insert str))
    (shell-command (format "sendtoiterm %s" tf))))

(defun iterm-send-line ()
  "Send line to iterm."
  (interactive)
  (iterm-send-string (thing-at-point 'line)))

(defun iterm-send-region (start end)
  "Send region to iterm."
  (interactive "r")
  (iterm-send-string (buffer-substring-no-properties start end)))

(defun iterm-send-paragraph ()
  "Send paragraph to iterm."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (iterm-send-region (point) (mark))
    (deactivate-mark)))
