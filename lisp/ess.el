;;; lisp/ess.el -*- lexical-binding: t; -*-

(use-package! ess
  :mode (("\\.[Rr]\\'" . ess-r-mode)
         ("\\.Rprofile\\'" . ess-r-mode))
  :custom
  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:fun-calls . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:constants . t)
     (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:%op% . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers)
     (ess-fl-keyword:operators)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T)))
  :config
  (add-hook! 'ess-r-mode-hook
             #'turn-off-smartparens-mode
             #'electric-pair-local-mode
             #'outline-minor-mode)
  ;; (add-hook! '(ess-r-mode-hook inferior-ess-r-mode-hook) (eldoc-mode -1))
  (setq comint-move-point-for-output t
        ess-ask-for-ess-directory nil
        ess-directory-function #'projectile-project-root
        ess-style 'RStudio
        ess-history-directory (expand-file-name "ess-history" doom-cache-dir)
        ess-roxy-str "#'"
        inferior-R-args "--no-save --no-restore")
  (setq flycheck-lintr-linters "with_defaults(line_length_linter(120), object_name_linter = NULL)")
  (setq-local outline-regexp "###+ "
              outline-heading-end-regexp "\n")
  (f-mkdir ess-history-directory)
  (put 'ess-r-package-dirs 'safe-local-variable #'listp)
  (advice-add #'ess-eval-region :override #'ans/r-send-region-source)
  (map! (:map ess-mode-map
         :g "M-RET" nil
         (:localleader
          :n "r" nil
          :n "rf" #'ess-switch-process
          :n "rq" #'ess-quit
          :n "cd" #'ess-use-this-dir
          :n "cD" #'ess-use-dir))

        (:map ess-r-mode-map
         :i "_" #'self-insert-command
         :i "M--" #'ess-insert-assign
         :i "M-m" (cmd! (insert " %>%"))
         :i "M-f" (cmd! (insert " %<-%"))
         :i "C-'" (cmd! (insert "#'"))
         :n "g RET" #'ess-eval-line-and-step

         (:localleader
          "v" #'ess-r-package-dev-map
          "p" nil)

         (:localleader
          :n "pp" #'ess-eval-paragraph
          :n "pd" #'ess-eval-paragraph-and-step
          :n "pl" #'ans/ess-eval-pipe-through-line
          :n "aa" #'ess-eval-buffer
          :n "as" #'ess-eval-buffer-from-beg-to-here
          :n "ad" #'ess-eval-buffer-from-here-to-end
          :n "rp" #'ans/ess-eval-symbol
          :n "rg" #'ans/ess-glimpse-symbol
          :n "rs" #'ans/ess-str
          :n "rz" #'ans/ess-symbol-size
          :n "rn" #'ans/ess-names
          :n "rH" #'ans/ess-head
          :n "rT" #'ans/ess-tail
          :n "kr" #'ans/rmarkdown-render
          :n "kk" #'ans/knitr-knit
          :n "X " #'ess-interrupt
          :n "0" #'ans/ess-dev-off
          :n "!" #'ans/ess-toggle-debug
          :n "=" (cmd! (ess-send-string (ess-get-process) "dev.size()" 'nowait))
          :n "zp" #'ans/ess-usethis-package
          :n "rl" #'ans/ess-drake-loadd
          :n "rd" #'ans/ess-drake-readd
          :n "rb" #'ans/ess-drake-build-target
          :n "ri" (lambda (arg) (interactive "P") (ans/import/add-to-imports arg) (ans/import/source-imports))
          :n "," #'ans/split-path-string
          :n "$" #'ans/ess-dollar-to-bracket
          :n "<backspace>" #'ans/ess-rm
          :n "bt" #'ans/ess-trace-back
          :n "bT" #'ans/ess-last-error
          :v "rx" #'ans/ess-reprex-region
          :v "ss" #'ess-eval-region
          :v "sd" #'ess-eval-region-or-function-or-paragraph-and-step
          :v "sc" #'ans/ess-eval-chunk))

        (:map ess-r-package-dev-map
         :g "C" #'ans/ess-compile-attributes)

        (:map inferior-ess-mode-map
          :n "C-y" #'evil-scroll-line-up)))

(set-popup-rule!
  (rx string-start (zero-or-more blank)
      "*" (zero-or-more blank) "ess-output"
      (zero-or-more blank) "*")
  :vslot 3)

(defun ans/ess-eval-chunk ()
  "Evaluate current chunk"
  (interactive)
  (save-excursion
    (polymode-mark-or-extend-chunk)
    (ess-eval-region (point) (mark))
    (deactivate-mark)))

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

(defun ans/ess-eval-symbol ()
  "Evaluate (usually print) the symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "%s"))

(defun ans/ess-do-with-symbol-at-point (command-string)
  "Run R expression in COMMAND-STRING using symbol-at-point."
  (ess-send-string
   (ess-get-process)
   (format command-string (ess-symbol-at-point))
   'nowait))

(defun ans/ess-reprex-region (start end venue)
  "Run the selection through `reprex::reprex', saving the output to the clipboard."
  (interactive "r\nsVenue (gh, so, ds, or r): ")
  (simpleclip-copy start end)
  (ess-send-string (ess-get-process) (format "reprex::reprex(venue = '%s', show = FALSE)" venue)) 'nowait)

(defun ans/ess-compile-attributes ()
  "Run ~Rcpp::compileAttributes~ on the package in the current directory."
  (interactive)
  (ess-send-string
   (ess-get-process)
   "Rcpp::compileAttributes()"
   'nowait))

(defun ans/ess-symbol-size ()
  "Run 'pryr::object_size' on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "pryr::object_size(%s)"))

(defun ans/ess-head ()
  "Run 'base::head' on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "head(%s)"))

(defun ans/ess-tail ()
  "Run 'base::head' on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "tail(%s)"))

(defun ans/ess-toggle-debug ()
  "Toggle debugging function at point."
  (interactive)
  (ess-send-string
   (ess-get-process)
   (format
    "if (isdebugged(%1$s)) {\n message('Undebug %1$s') \n undebug(%1$s) \n} else { \n message('Debug %1$s') \n debug(%1$s)}"
    (symbol-at-point))))

(defun ans/ess-usethis-package ()
  "Run `usethis::use_package` on symbol at point."
  (interactive)
  (let (wrd pkg)
    (setq wrd (symbol-name (ess-symbol-at-point)))
    (setq pkg (car (s-split "::" wrd)))
    (ess-send-string
     (ess-get-process)
     (format "usethis::use_package('%s')" pkg)
     'nowait)))

(defun ans/rmarkdown-render ()
  "Render the current R markdown document."
  (interactive)
  (ess-send-string (ess-get-process) (format "rmarkdown::render('%s')" (buffer-file-name)) 'nowait))

(defun ans/knitr-knit ()
  "Knit the current R markdown document."
  (interactive)
  (ess-send-string (ess-get-process) (format "knitr::knit('%s')" (buffer-file-name)) 'nowait))

(defun ans/ess-glimpse-symbol ()
  "Run 'dplyr::glimpse' on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "dplyr::glimpse(%s)"))

(defun ans/ess-names ()
  "Run `names` on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "names(%s)"))

(defun ans/ess-str (arg)
  "Run `names` on symbol at point. Optional prefix argument sets `max.level`."
  (interactive "P")
  (let* ((m (if arg (number-to-string arg) "NA"))
         (do-string (concat "str(%s, max.level = " m ")")))
    (ans/ess-do-with-symbol-at-point do-string)))

(defun ans/ess-dev-off ()
  "Turn off the current graphics device."
  (interactive)
  (ess-send-string (ess-get-process) "dev.off()" 'nowait))

(defun ans/split-path-string ()
  "Split a single path string into a comma-separated list suitable for, e.g., `file.path'."
  (interactive)
  (let* (path bounds start end path-insert)
    ;; Use sexp here to include quotes
    (setq path (thing-at-point 'filename t))
    (if (not path) (cl-return nil))
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
    (if (not (length thing-split)) (cl-return nil))
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

(defun ans/ess-rm ()
  "Delete object at point from global environment."
  (interactive)
  (ans/ess-do-with-symbol-at-point "rm(%s)"))

(defun ans/ess-trace-back ()
  "Trace back the last error."
  (interactive)
  (ess-send-string (ess-get-process) "rlang::last_error()" 'nowait))

(defun ans/ess-last-error ()
  "Run `rlang::last_error'."
  (interactive)
  (ess-send-string (ess-get-process) "rlang::last_trace()") 'nowait)

(defun ans/ess-close-if-not-running (window)
  "Close R popup WINDOW if R process is not running."
  (when (not (ess-process-live-p))
    (delete-window window)))

(defun ans-r-file-here ()
  "Use here::here to determine path for R buffer."
  (shell-command-to-string
   (concat
    "Rscript -e \""
    "my_dir <- dirname('"(buffer-file-name)"');"
    "t <- tryCatch(setwd(my_dir), error = function(e) NULL);"
    "cat(here::here())"
    "\"")))

(defun ans/beginning-of-pipe-or-eol ()
  "Find point position of end of line or beginning of pipe."
  (if (search-forward "%>%" (line-end-position) t)
      (goto-char (match-beginning 0))
    (end-of-line)))

(defun ans/ess-eval-pipe-through-line ()
  "Eval from beginning of paragraph to this line's pipe, or if no pipe, end of paragraph."
  (interactive)
  (save-excursion
    (let ((end (progn
                 (ans/beginning-of-pipe-or-eol)
                 (point)))
          (beg (progn
                 (backward-paragraph)
                 (ess-skip-blanks-forward 'multiline)
                 (point))))
      (ans/r-send-region-source beg end))))

(defun ans/ess-drake-readd ()
  "Run `targets::tar_read` or `drake::readd` on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point
   "if (file.exists('_targets.R')) targets::tar_read('%1$s') else drake::readd('%1$s')"))

(defun ans/ess-drake-loadd ()
  "Run `targets::tar_load` or `drake::loadd` on symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point
   "if (file.exists('_targets.R')) targets::tar_load('%1$s') else drake::loadd('%1$s')"))

(defun ans/ess-drake-build-target ()
  "Build drake target symbol at point."
  (interactive)
  (ans/ess-do-with-symbol-at-point "invisible(drake::r_drake_build('%1$s')); drake::loadd('%1$s')"))

(defun ans/r-send-region-source (start end &optional vis message type)
  "Send region from START to END to R using source function."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end)))
    (ess-send-string
     (ess-get-process)
     (format "source(stdin(), echo = TRUE)
%s" code)
     vis message type)
    (process-send-eof (ess-get-process))))
