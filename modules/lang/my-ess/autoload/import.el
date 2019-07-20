;;; lang/my-ess/autoload/import.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ans/import/R-pkg-fun-word-at-point ()
  "Select a package::function statement as a word."
  (let (($temp-syn-table (make-syntax-table ess-r-mode-syntax-table)))
    (modify-syntax-entry ?: "w" $temp-syn-table)
    (modify-syntax-entry ?_ "w" $temp-syn-table)
    (with-syntax-table $temp-syn-table
      (thing-at-point 'word t))))

;;;###autoload
(defun ans/import/R-pkg-delete-namespace ()
  "Replace package::function with just function."
  (interactive)
  (save-excursion
   (let (($temp-syn-table (make-syntax-table ess-r-mode-syntax-table)))
     (modify-syntax-entry ?: "w" $temp-syn-table)
     (modify-syntax-entry ?_ "w" $temp-syn-table)
     (with-syntax-table $temp-syn-table
       (let* ((bounds (bounds-of-thing-at-point 'word))
              (start (car bounds))
              (end (cdr bounds))
              (thing (ans/import/R-pkg-fun-word-at-point))
              (fun (nth 1 (ans/import/split-pkg-fun-call thing))))
         (delete-region start end)
         (goto-char start)
         (insert fun))))))

;;;###autoload
(defun ans/import/split-pkg-fun-call (word)
  "Split WORD containing package::function into package and function."
  (s-split "::" word))

;;;###autoload
(defun ans/import/import-to-list (import-string)
  "Grab IMPORT-STRING and convert to a list.

  The first element of this list is the package and the remaining statements are imports."
  (let* ((import-list (s-with import-string
                        (s-chop-prefix "import::from(")
                        (s-chop-suffix ")")
                        (s-split ", +")))
         (no-into (-remove (lambda (s) (s-matches? "\.into +=" s)) import-list)))
    (mapcar (lambda (s) (s-with s (s-chop-prefix "\"") (s-chop-suffix "\""))) no-into)))

;;;###autoload
(defun ans/import/find-import-lists ()
  "Find the nearest imports list from the current buffer, and return
    their start and end points as a list.

  Imports lists start with the comment string 'begin imports' and end
  with 'end imports'."
  (let* ((import-start (save-excursion
                         (re-search-backward "#+ *begin imports" nil t)
                         (forward-line)
                         (point)))
         (import-end (save-excursion
                       (goto-char import-start)
                       (re-search-forward "#+ *end imports" nil t)
                       (forward-line -1)
                       (end-of-line)
                       (point))))
    (when (and import-start import-end)
      (list import-start import-end))))

;;;###autoload
(defun ans/import/parse-import-lists (import-block-string)
  "Convert block of import statements into a list of import strings.

  In the process, remove extraneous whitespace."
  (let* ((import-block-list
          (s-with import-block-string
            (s-collapse-whitespace)
            (s-match-strings-all "import::from(.*?)"))))
    (-map 'ans/import/import-to-list (-flatten import-block-list))))

;;;###autoload
(defun ans/import/read-imports ()
  "Read and parse current buffer's imports block."
  (let* ((import-start-end (ans/import/find-import-lists))
         (import-block (buffer-substring-no-properties
                        (nth 0 import-start-end)
                        (nth 1 import-start-end))))
    (ans/import/parse-import-lists import-block)))

;;;###autoload
(defun ans/import/list-to-import (import-list)
  "Convert import-list to a formatted import string."
  (let* ((import-list-q (mapcar (lambda (s) (s-wrap s "\"")) import-list))
         (imports-joined (s-join ", " import-list-q)))
    (s-with imports-joined (s-prepend "import::from(") (s-append ", .into = \"\")"))))

;;;###autoload
(defun ans/import/replace-imports (import-start-end new-imports)
  "Replace imports list at IMPORT-START-END with NEW-IMPORTS."
  (delete-region (nth 0 import-start-end) (nth 1 import-start-end))
  (let* ((import-string-list (-map 'ans/import/list-to-import new-imports))
         (import-string (s-join "\n" import-string-list))
         (start (nth 0 import-start-end))
         (end (nth 1 import-start-end)))
    (save-excursion
      (goto-char start)
      (insert import-string)
      (indent-region start (max end (point-at-eol))))))

;;;###autoload
(defun ans/import/search-function (fun)
  "Search for R packages containing FUN."
  (let* ((search-command (format "out <- help.search('^%s$', fields = c('name', 'alias'), ignore.case = TRUE)" fun))
         (result-command "cat(with(out[['matches']], unique(sprintf('%s::%s', Package, Entry))))")
         (complete-command (concat "Rscript -e \"" search-command ";" result-command "\""))
         (raw-result (shell-command-to-string complete-command))
         (split-result (s-split " " raw-result)))
    (if (> (length split-result) 1)
        (completing-read "Which package? " split-result)
      (car split-result))))

;;;###autoload
(defun ans/import/add-to-imports (arg)
  "Add the current thing to the imports list and update the imports list.
Unless called with prefix argument ARG, also delete the namespace from the current call.

If the thing at point is just a bare function without a namespace, try to identify the function's package."
  (interactive "P")
  (let* (fun addition is_namespaced)
    (setq fun (ans/import/R-pkg-fun-word-at-point))
    (setq is_namespaced (s-contains? "::" fun))
    ;; If it's just a function, try to find it on the search path
    (when (not is_namespaced)
      (setq fun (ans/import/search-function fun)))
    (setq addition (ans/import/split-pkg-fun-call fun))
    ;; If current word isn't parse-able as "package::function", do nothing.
    (when (= (length addition) 2)
      ;; Replace "package::function" with just "function", unless called
      ;; with a prefix argument.
      (when (and (not arg) is_namespaced) (ans/import/R-pkg-delete-namespace))
      (let ((current-imports (ans/import/read-imports))
            (import-bounds (ans/import/find-import-lists)))
        (if (= (length current-imports) 0)
            ;; There are no current imports. Just add it!
            (ans/import/replace-imports import-bounds (list addition))
          ;; There are existing imports
          (let* ((current-packages (mapcar 'car current-imports))
                 (addition-package (nth 0 addition))
                 (i-current-package (-find-index (-partial 's-equals? addition-package) current-packages))
                 (new-imports
                  (if (not (-contains? current-packages addition-package))
                      ;; No other functions imported from this package
                      ;; yet. Just append to the current imports list.
                      (append current-imports (list addition))
                    (let* ((new-function (nth 1 addition))
                           (add-to-package (nth i-current-package current-imports)))
                    ;; At least one function has been imported. Is it
                    ;; this one? (If so, no need to add, so do nothing.)
                      (if (not (-contains? add-to-package new-function))
                          ;; Append the current function to the
                          ;; current package list.
                          (-replace-at
                           i-current-package
                           (append add-to-package (list new-function)) current-imports)
                        current-imports)))))
            (ans/import/replace-imports (ans/import/find-import-lists) new-imports)))))))

;;;###autoload
(defun ans/import/source-imports ()
  "Source the current imports block."
  (interactive)
  (save-excursion
    (let* (start-end start end)
      (setq start-end (ans/import/find-import-lists))
      (setq start (nth 0 start-end))
      (setq end (nth 1 start-end))
      (ans/ess-tidy-region start (+ end 1))
      ;; Have to do it again because region has changed
      (setq start-end (ans/import/find-import-lists))
      (setq start (nth 0 start-end))
      (setq end (nth 1 start-end))
      (ess-send-region (ess-get-process) start end nil "Re-loaded imports"))))
