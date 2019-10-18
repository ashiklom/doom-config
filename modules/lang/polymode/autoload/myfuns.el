;;; lang/polymode/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ans/poly-to-markdown-mode ()
  "Switch from polymode to markdown mode and kill polymode buffers."
  (interactive)
  (markdown-mode)
  (ans/kill-polymode-buffers))

;;;###autoload
(defun ans/toggle-poly-markdown-mode ()
  "Toggle poly-markdown mode."
  (interactive)
  (if (and (boundp 'poly-markdown-mode) poly-markdown-mode)
      (ans/poly-to-markdown-mode)
    (poly-markdown-mode)))

;;;###autoload
(defun ans/kill-polymode-buffers ()
  "List all polymode implementation buffers."
  (interactive)
  (let* ((b (buffer-name))
         (pattern (concat b "\\[.+\\]"))
         (matched-buffers (-filter (lambda (buf) (s-matches? pattern (buffer-name buf))) (buffer-list)))
         (n-matched (length matched-buffers)))
    (-map #'kill-buffer matched-buffers)
    (message "Killed %d polymode buffers." n-matched)))

;;;###autoload
(defun ans/reload-polymode ()
  "Revert buffer and load poly-markdown-mode."
  (interactive)
  (save-buffer)
  (revert-buffer nil t)
  (poly-markdown-mode 1))

;;;###autoload
(defun ans/insert-chunk-and-enter (chunktype)
  "Insert chunk of type CHUNKTYPE and enter it."
  (insert "```" chunktype "\n\n```")
  (previous-line)
  (beginning-of-line))

;;;###autoload
(defun ans/poly-split-chunk-here ()
  "Split chunk into two chunks at point."
  (interactive)
  (beginning-of-line)
  (insert "```\n\n```")
  (previous-line)
  (beginning-of-line))
