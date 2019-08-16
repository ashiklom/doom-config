;;; lang/polymode/config.el -*- lexical-binding: t; -*-

(use-package! polymode)
(use-package! poly-markdown
  :commands (poly-markdown-mode)
  :config
  (map! :map poly-markdown-mode-map
        :localleader
        :n "P" #'ans/toggle-poly-markdown-mode
        :n "R" #'ans/reload-polymode
        :n "`" #'ans/chunk-hydra/body
        :i "C-'" #'ans/chunk-hydra/body))

(use-package! fence-edit
  :config
  (add-to-list 'fence-edit-blocks '("^```{r.*}" "^```$" R))
  (add-to-list 'fence-edit-blocks '("^```{tikz.*}" "^```$" latex))
  (map! (:map markdown-mode-map
          :localleader
          :n "e" #'fence-edit-code-at-point)
        (:map fence-edit-mode-map
          :prefix "C-c"
          "C-c" #'fence-edit-exit
          "C-k" #'fence-edit-abort)))

(use-package! markdown-mode
  :config
  (map! :map markdown-mode-map
        :localleader
        :desc "Polymode" :n "P" #'poly-markdown-mode))

(defhydra ans/chunk-hydra (:exit t)
  "Insert chunks."
  ("`" (ans/insert-chunk-and-enter "") "Plain")
  ("r" (ans/insert-chunk-and-enter "{r}") "R (knitr)")
  ("R" (ans/insert-chunk-and-enter "r") "R (plain)")
  ("-" ans/poly-split-chunk-here "Split")
  ("SPC" polymode-toggle-chunk-narrowing))
