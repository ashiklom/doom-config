;; -*- no-byte-compile: t; -*-
;;; tools/bibtex/packages.el

(package! bibtex-completion :recipe (:host github :repo "tmalsburg/helm-bibtex" :files ("bibtex-completion.el")))
(package! ivy-bibtex)
(package! org-ref)

(package! citeproc :disable t)
(package! citeproc-org :recipe (:host github :repo "andras-simonyi/citeproc-org") :disable t)
