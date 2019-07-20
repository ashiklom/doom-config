;;; lang/my-ess/autoload/send-source.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ans/r-send-region-source (start end)
  "Send region from START to END to R using source function."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (nlines (count-lines start end)))
    (ess-send-string
     (ess-get-process)
     (format "
.esstmpfile <- tempfile()
.essstring <- readLines(con = stdin(), n = %d)
%s
writeLines(.essstring, .esstmpfile)
source(.esstmpfile, echo = TRUE)
" nlines code))))

;;;###autoload
(defun ans/r-send-line ()
  "Send current line to R."
  (interactive)
  (ans/r-send-region-source (line-beginning-position) (line-end-position)))

;;;###autoload
(defun ans/r-send-buffer ()
  "Send entire current buffer to R."
  (interactive)
  (ans/r-send-region-source (point-min) (point-max)))

;;;###autoload
(defun ans/r-send-beginning-to-current-line ()
  "Run from beginning of buffer through end of current line."
  (interactive)
  (ans/r-send-region-source (point-min) (line-end-position)))

;;;###autoload
(defun ans/r-send-current-line-to-end ()
  "Run from beginning of current line to end of buffer."
  (interactive)
  (ans/r-send-region-source (line-beginning-position) (point-max)))

;;;###autoload
(defun ans/r-send-function-or-paragraph ()
  "Send current function definition."
  (interactive)
  (save-excursion
    (ess-mark-function-or-para)
    (ans/r-send-region-source (point) (mark))
    (deactivate-mark)))

;;;###autoload
(defun ans/r-send-paragraph ()
  "Send current paragraph."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (ans/r-send-region-source (point) (mark))
    (deactivate-mark)))

;;;###autoload
(defun ans/r-send-line-and-down ()
  "Send current line to R and advance one line down."
  (interactive)
  (ans/r-send-line)
  (ess-next-code-line))
