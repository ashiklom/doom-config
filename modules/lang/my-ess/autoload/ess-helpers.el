;;; lang/my-ess/autoload/ess-tidy-region.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ans/ess-tidy-region (start end)
  "Run formatr::tidy_source() on REGION."
  (interactive "r")
  (let* (tmp fixed)
    (setq tmp (make-temp-file "r-tidy"))
    (write-region start end tmp)
    (setq fixed (shell-command-to-string (format "Rscript -e 'formatR::tidy_source(\"%s\", width.cutoff = 60)'" tmp)))
    (delete-region start end)
    (goto-char start)
    (insert fixed)
    (delete-trailing-whitespace start end)
    (indent-region start end)))

;;;###autoload
(defun ans/ess-eval-symbol ()
  "Evaluate (usually print) the symbol at point."
  (interactive)
  (save-excursion
    (er/mark-symbol)
    (ess-eval-region (point) (mark) nil)
    (deactivate-mark)))

;;;###autoload
(defun ans/ess-do-with-symbol-at-point (command-string)
  "Run R expression in COMMAND-STRING using symbol-at-point."
  (ess-send-string
   (ess-get-process)
   (format command-string (symbol-at-point))))

;;;###autoload
(defun ans/ess-reprex-region (start end venue)
  "Run the selection through `reprex::reprex', saving the output to the clipboard."
  (interactive "r\nsVenue (gh, so, ds, or r): ")
  (simpleclip-copy start end)
  (ess-send-string (ess-get-process) (format "reprex::reprex(venue = '%s', show = FALSE)" venue)))

;;;###autoload
(defun ans/ess-compile-attributes ()
  "Run ~Rcpp::compileAttributes~ on the package in the current directory."
  (interactive)
  (ess-send-string
   (ess-get-process)
   "Rcpp::compileAttributes()"))

;;;###autoload
(defun ans/ess-symbol-size ()
  "Run 'pryr::object_size' on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "pryr::object_size(%s)"))

;;;###autoload
(defun ans/ess-head ()
  "Run 'base::head' on symbol at point."
  (interactive)
  (ess-send-string (ess-get-process) (format "head(%s)" (symbol-at-point))))

;;;###autoload
(defun ans/ess-tail ()
  "Run 'base::head' on symbol at point."
  (interactive)
  (ess-send-string (ess-get-process) (format "tail(%s)" (symbol-at-point))))

;;;###autoload
(defun ans/ess-toggle-debug ()
  "Toggle debugging function at point."
  (interactive)
  (ess-send-string
   (ess-get-process)
   (format
    "if (isdebugged(%1$s)) {\n message('Undebug %1$s') \n undebug(%1$s) \n} else { \n message('Debug %1$s') \n debug(%1$s)}"
    (symbol-at-point))))

;;;###autoload
(defun ans/ess-usethis-package ()
  "Run `usethis::use_package` on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "usethis::use_package('%s')"))

;;;###autoload
(defun ans/rmarkdown-render ()
  "Render the current R markdown document."
  (interactive)
  (ess-send-string (ess-get-process) (format "rmarkdown::render('%s')" (buffer-file-name))))

;;;###autoload
(defun ans/ess-glimpse-symbol ()
  "Run 'dplyr::glimpse' on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "dplyr::glimpse(%s)"))

;;;###autoload
(defun ans/ess-names ()
  "Run `names` on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "names(%s)"))

;;;###autoload
(defun ans/ess-str (arg)
  "Run `names` on symbol at point. Optional prefix argument sets `max.level`."
  (interactive "P")
  (let* ((m (if arg (number-to-string arg) "NA"))
         (do-string (concat "str(%s, max.level = " m ")")))
    (ans/ess-do-with-symbol-at-point do-string)))

;;;###autoload
(defun ans/ess-dev-off ()
  "Turn off the current graphics device."
  (interactive)
  (ess-send-string
   (ess-get-process)
   "dev.off()"))

;;;###autoload
(defun ans/split-path-string ()
  "Split a single path string into a comma-separated list suitable for, e.g., `file.path'."
  (interactive)
  (let* (path bounds start end path-insert)
    ;; Use sexp here to include quotes
    (setq path (thing-at-point 'filename t))
    (if (not path) (return nil))
    (setq bounds (bounds-of-thing-at-point 'filename))
    (setq start (- (car bounds) 1))
    (setq end (+ (cdr bounds) 1))
    (setq path-insert (s-with path
                        (s-chop-prefix "\"")
                        (s-chop-suffix "\"")
                        (s-split "/")
                        (mapcar (lambda (s) (s-wrap s "\"")))
                        (s-join ", ")))
    (delete-region start end)
    (goto-char start)
    (insert path-insert)))

;;;###autoload
(defun ans/ess-dollar-to-bracket ()
  "Convert `$' subsetting notation to `[['."
  (interactive)
  (let* (thing thing-split thing-car thing-cdr thing-cdr-q thing-cdr-combined
               insert-string start end)
    ;; TODO: Refine this logic to select the entire symbol. Currently,
    ;; only selects until the next `$' (or other end-of-word character).
    (setq start (save-excursion (backward-word-begin) (point)))
    (setq end (save-excursion (evil-forward-word-end) (+ (point) 1)))
    (setq thing (buffer-substring-no-properties start end))
    (setq thing-split (s-split "\\$" thing))
    (if (not (length thing-split)) (return nil))
    (setq thing-car (car thing-split))
    (setq thing-cdr (cdr thing-split))
    (setq thing-cdr-q (mapcar (lambda (s) (s-wrap s "\"")) thing-cdr))
    (setq thing-cdr-combined (s-join ", " thing-cdr-q))
    (if (> (length thing-cdr) 1)
        (setq thing-cdr-combined (s-wrap thing-cdr-combined "c(" ")")))
    (setq insert-string (format "%s[[%s]]" thing-car thing-cdr-combined))
    (delete-region start end)
    (goto-char start)
    (insert insert-string)))

;;;###autoload
(defun ans/ess-plot-imgur (start end)
  "Run plot code with imguR device."
  (interactive "r")
  (let* (command string)
    (setq command (format ".imgur <- imguR::imgur('png', width = %s, height = %s, units = 'in', res = 300)"
                          (read-string "Width [5]: " nil nil "5") (read-string "Height [5]: " nil nil "5")))
    (setq string (s-join "\n" (list command
                                    (buffer-substring-no-properties start end)
                                    "imguR::imgur_off(.imgur)$link")))
    (ess-send-string (ess-get-process) string)))

;;;###autoload
(defun ans/ess-rm ()
  "Delete object at point from global environment."
  (interactive)
  (ans/ess-do-with-symbol-at-point "rm(%s)"))

;;;###autoload
(defun ans/ess-trace-back ()
  "Trace back the last error."
  (interactive)
  (ess-send-string (ess-get-process) "rlang::last_error()"))

;;;###autoload
(defun ans/ess-last-error ()
  "Run `rlang::last_error'."
  (interactive)
  (ess-send-string (ess-get-process) "rlang::last_trace()"))

;;;###autoload
(defun ans/ess-close-if-not-running (window)
  "Close R popup WINDOW if R process is not running."
  (when (not (ess-process-live-p))
    (delete-window window)))

;;;###autoload
(defun ans-r-file-here ()
  "Use here::here to determine path for R buffer."
  (shell-command-to-string
   (concat
    "/usr/local/bin/Rscript -e \""
    "my_dir <- dirname('"(buffer-file-name)"');"
    "t <- tryCatch(setwd(my_dir), error = function(e) NULL);"
    "cat(here::here())"
    "\"")))
