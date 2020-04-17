;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! multi-line)
(package! deadgrep)
(package! dtrt-indent)
(package! pandoc-mode)
(package! simpleclip)

(unpin! flycheck)

(package! lsp-julia :recipe (:host github :repo "non-Jedi/lsp-julia"))
(unpin! lsp-julia)

(package! julia-repl :recipe (:host github :repo "tpapp/julia-repl" :branch "tp/terminal-backends"))
(unpin! julia-repl)
