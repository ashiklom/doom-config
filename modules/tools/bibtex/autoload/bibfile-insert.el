;;; tools/bibtex/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ans/biblio-selection-insert-end-of-bibfile ()
  "Insert BibTeX of current entry at the end of my library.bib file."
  (interactive)
  (biblio--selection-forward-bibtex #'ans/biblio--selection-insert-at-end-of-bibfile-callback))

;;;###autoload
(defun ans/biblio--selection-insert-at-end-of-bibfile-callback (bibtex entry)
  "Add BIBTEX (from ENTRY) to end of library.bib file."
  (with-current-buffer (find-file-noselect ans/reference-bibfile)
    (goto-char (point-max))
    (newline)
    (insert bibtex)
    ;; (bibtex-clean-entry 4)
    (save-buffer))
  (message "Inserted bibtex entry for %S."
           (biblio--prepare-title (biblio-alist-get 'title entry))))
