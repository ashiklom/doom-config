;;; tools/bibtex/config.el -*- lexical-binding: t; -*-

(use-package! ivy-bibtex
  :commands (ivy-bibtex)
  :config
  (setq ivy-bibtex-default-action #'ivy-bibtex-insert-citation)
  (defun bibtex-completion-fallback-candidates ()
    "Custom list of bibtex fallback options. This is the same as the
one that ships with helm-bibtex, except that
`bibtex-completion-fallback-options` comes first."
    (let ((bib-files (bibtex-completion-normalize-bibliography 'main)))
      (-concat
       bibtex-completion-fallback-options
       (--map (cons (s-concat "Create new entry in " (f-filename it))
                    `(lambda (_search-expression) (find-file ,it) (goto-char (point-max)) (newline)))
              bib-files))))
  (advice-add 'bibtex-completion-candidates
              :filter-return 'reverse))

(use-package! org-ref
  :init
  (setq bibtex-completion-additional-search-fields '(journal keywords)
        bibtex-completion-display-formats
        '((t . "${author:36} ${journal:30} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${keywords:20}")))
  :commands org-ref)

(use-package! citeproc-org
  :after org
  :commands citeproc-org-setup
  :init
  (setq citeproc-org-ignore-backends nil))
