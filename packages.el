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
(unpin! ess)

(package! julia-repl :recipe (:host github :repo "tpapp/julia-repl"))
(unpin! julia-repl)

(package! stan-mode)
(package! company-stan)
(package! eldoc-stan)
(package! flycheck-stan)

(package! vimrc-mode)

(package! websocket :recipe (:host github :repo "ahyatt/emacs-websocket" :branch "main"))
