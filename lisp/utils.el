;;; ~/.doom.d/autoloads/utils.el -*- lexical-binding: t; -*-

;; Some custom functions
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun ans/delete-file-and-buffer ()
  "Kill the current buffer and delete the associated file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (progn
        (delete-file filename)
        (message "Deleted file %s" filename)
        (kill-buffer)))))

;; Automatically commenting with "o/O" is sometimes useful, but usually just
;; annoying. So, do it with "go/O", but not with regular "o/O".
(defun ans/evil-open-below (count)
  "`(evil-open-below COUNT)` without the extra advice that adds comments."
  (interactive "p")
  (evil-open-below count))

(defun ans/evil-open-above (count)
  "`(evil-open-below COUNT)` without the extra advice that adds comments."
  (interactive "p")
  (evil-open-above count))

(defun ans/ctbl-sort-current-column ()
  "Sort the column of the currently selected cell in ctbl."
  (interactive)
  (let* ((cp  (ctbl:cp-get-component))
         (cell (ctbl:cp-get-selected cp))
         (j (cdr cell)))
    (ctbl:cmodel-sort-action cp j)))

(defun ans/chmod-this-file ()
  "Set the mode of the current file"
  (interactive)
  (chmod (buffer-file-name) (read-file-modes)))

; HACK: Manually redefine this to preserve original behavior
; See: https://github.com/emacs-ess/ESS/issues/1115
(defun newline-and-indent (&optional arg)
  "Insert a newline, then indent according to major mode.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the function `current-left-margin'.

With ARG, perform this action that many times."
  (interactive "*p")
  (delete-horizontal-space t)
  (unless arg
    (setq arg 1))
    (dotimes (_ arg)
      (newline nil t)
      (indent-according-to-mode)))
