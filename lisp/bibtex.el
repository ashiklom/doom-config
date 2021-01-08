;;; ~/.doom.d/+bibtex.el -*- lexical-binding: t; -*-

;;  Related to bibtex references
(defvar ans/reference-dir (file-name-as-directory (concat ans/dropbox-dir "references"))
  "Root directory for storing my bibliography.")
(defvar ans/reference-bibfile "~/Dropbox/references/zotero-library.bib"
  "Main bibliography file.")
(defvar ans/reference-notes (concat ans/reference-dir "notes.org")
  "References notes file.")
(defvar ans/reference-pdfs (file-name-as-directory (concat ans/reference-dir "pdfs"))
  "References PDFs directory.")

;; CSL locale files. If this doesn't exist, clone them from:
;; https://github.com/citation-style-language/locales
(setq citeproc-org-locales-dir "~/.config/csl-locales")

(defvar ans/bibtex-display-format
  '((t . "${author:36} ${journal:30} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${keywords:20}"))
  "My custom display format.")
(setq bibtex-completion-display-formats ans/bibtex-display-format)

(use-package! ivy-bibtex
  :commands ivy-bibtex
  :config
  (setq bibtex-completion-library-path ans/reference-pdfs
        bibtex-completion-notes-path ans/reference-notes
        bibtex-completion-bibliography ans/reference-bibfile
        bibtex-autokey-titleword-length 15
        org-ref-default-bibliography (list bibtex-completion-bibliography)
        org-ref-get-pdf-filename-function #'org-ref-get-pdf-filename-helm-bibtex)
  (setq bibtex-autokey-name-case-convert-function 'downcase
        bibtex-autokey-name-year-separator "_"
        bibtex-autokey-year-title-separator "_"
        bibtex-autokey-year-length 4
        bibtex-autokey-titlewords 1
        bibtex-autokey-titleword-ignore nil
        bibtex-autokey-titleword-case-convert-function 'downcase
        bibtex-completion-additional-search-fields '(journal keywords)
        bibtex-completion-display-formats ans/bibtex-display-format
        bibtex-completion-notes-template-one-file
        (string-join
         '("** TODO ${author-abbrev} (${year}): ${title}"
           ":PROPERTIES:"
           ":Custom_ID: ${=key=}"
           ":AUTHOR: ${author-or-editor}"
           ":FULL_TITLE: ${title}"
           ":JOURNAL: ${journal}"
           ":YEAR: ${year}"
           ":VOLUME: ${volume}"
           ":PAGES: ${pages}"
           ":DOI: ${doi}"
           ":URL: ${url}"
           ":END:"
           "cite:${=key=}") "\n")
        ; This is a major performance bottleneck
        bibtex-completion-pdf-field nil
        bibtex-completion-pdf-open-function #'counsel-find-file-extern)
  (map! (:map biblio-selection-mode-map
          :desc "Add to bibfile" "I" #'ans/biblio-selection-insert-end-of-bibfile)
        (:map org-mode-map
          :localleader
          (:prefix-map ("\\" . "org-ref")
           :desc "URL" "u" #'org-ref-open-url-at-point))))

(after! bibtex
  :config
  (map!
   (:map bibtex-mode-map
    :prefix "C-c"
    :desc "Insert DOI" "i" #'doi-insert-bibtex
    :desc "Insert DOI from clipboard" "I" (lambda () (interactive) (doi-insert-bibtex (x-get-clipboard)))
    :desc "Format entry" "l" (lambda () (interactive) (bibtex-clean-entry 4))
    :desc "Edit notes" "n" (lambda () (interactive) (bibtex-completion-edit-notes (list (bibtex-completion-key-at-point))))
    :desc "Open URL" "u" (lambda () (interactive (bibtex-completion-open-url-or-doi (list (bibtex-completion-key-at-point))))))))

(use-package! org-ref
  :when (featurep! :lang org)
  :after (org bibtex-completion)
  :config
  (setq org-ref-bibliography-notes ans/reference-notes
        org-ref-default-bibliography (list ans/reference-bibfile)
        reftex-default-bibliography org-ref-default-bibliography
        org-ref-pdf-directory ans/reference-pdfs
        ;; This is a huge performance bottleneck if enabled.
        ;; Can quickly view broken links with `M-x org-ref'
        org-ref-show-broken-links nil))

(defun ans/hsplit-frame ()
  "Split window entirely below the current frame."
  (split-window (frame-root-window) nil 'below))

(after! bibtex-completion
  (defun bibtex-completion-format-citation-orgref (keys)
    "Format org-ref citation references for keys in KEYS."
    (s-join ", "
            (--map (format "cite:%s" it) keys))))
