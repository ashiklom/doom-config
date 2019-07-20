;;; lang/ess/config.el -*- lexical-binding: t; -*-

(def-package! ess
  :commands (stata SAS)
  :init
  (setq ess-smart-S-assign-key nil)
  (unless (featurep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode)))
  :config
  (set-popup-rule!
    (rx string-start "*" "R" (any "*" ":"))
    :quit #'ans/ess-close-if-not-running
    :size 0.3 :select nil)
  (setq ess-offset-continued 'straight
        ess-use-flymake (not (featurep! :tools flycheck))
        ess-ask-for-ess-directory nil
        ess-directory-function #'ans-r-file-here
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'RStudio
        ess-eval-visibly 'nowait
        ess-history-directory (expand-file-name "ess-history/" doom-cache-dir)
        ess-roxy-str "#'"
        ess-roxy-fill-param-p t
        ess-roxy-template-alist
        '(("description" . ".. title/description ..")
          ("param" . "")
          ("return" . "")
          ("author" . "Alexey Shiklomanov"))
        inferior-R-args "--no-save --no-restore")

  (setf (alist-get 'ess-fl-keyword:fun-calls ess-R-font-lock-keywords) t)

  (set-repl-handler! '(ess-r-mode ess-julia-mode) #'+ess-repl-buffer)
  (set-lookup-handlers! '(ess-r-mode ess-julia-mode)
    :documentation #'ess-display-help-on-object)

  (set-evil-initial-state! 'ess-r-help-mode 'normal)
  (set-eval-handler! 'ess-help-mode #'ess-eval-region-and-go)
  (set-eval-handler! 'ess-r-help-mode #'ess-eval-region-and-go)

  (map! (:after ess-help
          :map ess-help-mode-map
          :n "q"  #'kill-current-buffer
          :n "Q"  #'ess-kill-buffer-and-go
          :n "K"  #'ess-display-help-on-object
          :n "go" #'ess-display-help-in-browser
          :n "gO" #'ess-display-help-apropos
          :n "gv" #'ess-display-vignettes
          :m "]]" #'ess-skip-to-next-section
          :m "[[" #'ess-skip-to-previous-section
          :map ess-doc-map
          "h" #'ess-display-help-on-object
          "p" #'ess-R-dv-pprint
          "t" #'ess-R-dv-ctable
          [C-return] #'ess-eval-line
          [up]       #'comint-next-input
          [down]     #'comint-previous-input)

        :localleader
        :map ess-mode-map
        "r f" #'R
        "r q" #'ess-quit
        "l" #'ans/r-send-line
        "d" #'ans/r-send-line-and-down
        "f f" #'ans/r-send-function-or-paragraph
        "p p" #'ans/r-send-paragraph
        "p d" #'ans/r-send-paragraph-and-down
        "a a" #'ans/r-send-buffer
        "a d" #'ans/r-send-current-line-to-end
        "a s" #'ans/r-send-beginning-to-current-line
        "r p" #'ans/ess-eval-symbol
        "r g" #'ans/ess-glimpse-symbol
        "r s" #'ans/ess-str
        "r z" #'ans/ess-symbol-size
        "r n" #'ans/ess-names
        "r H" #'ans/ess-head
        "r T" #'ans/ess-tail
        "k r" #'ans/rmarkdown-render
        "X " #'ess-interrupt
        "0" #'ans/ess-dev-off
        "!" #'ans/ess-toggle-debug
        "z p" #'ans/ess-usethis-package
        "r l" #'ans/ess-drake-loadd
        "r d" #'ans/ess-drake-readd
        "r b" #'ans/ess-drake-build-target
        "r i" (lambda (arg) (interactive "P") (ans/import/add-to-imports arg) (ans/import/source-imports))
        "," #'ans/split-path-string
        "$" #'ans/ess-dollar-to-bracket
        "<backspace>" #'ans/ess-rm
        "b t" #'ans/ess-trace-back
        "b T" #'ans/ess-last-error

        :v "s s" #'ans/r-send-region-source
        :v "s i" #'ans/ess-plot-imgur
        :v "r x" #'ans/ess-reprex-region
        :v "=" #'ans/ess-tidy-region

        :i "_" #'self-insert-command
        :i "M--" #'ess-insert-assign
        :i "M-m" (lambda() (interactive)(insert " %>%"))

        ;; Old bindings
        ;; "," #'ess-eval-region-or-function-or-paragraph-and-step
        ;; [tab]     #'ess-switch-to-inferior-or-script-buffer
        ;; [backtab] #'ess-switch-process
        ;; REPL
        ;; "B" #'ess-eval-buffer-and-go
        ;; "b" #'ess-eval-buffer
        ;; "d" #'ess-eval-region-or-line-and-step
        ;; "D" #'ess-eval-function-or-paragraph-and-step
        ;; "L" #'ess-eval-line-and-go
        ;; "l" #'ess-eval-line
        ;; "R" #'ess-eval-region-and-go
        ;; "F" #'ess-eval-function-and-go
        ;; "f" #'ess-eval-function
        ;; predefined keymaps
        "h" 'ess-doc-map
        "x" 'ess-extra-map
        "v" 'ess-r-package-dev-map
        "e" 'ess-dev-map
        ;; noweb
        ;; :prefix "c"
        ;; "C" #'ess-eval-chunk-and-go
        ;; "c" #'ess-eval-chunk
        ;; "d" #'ess-eval-chunk-and-step
        ;; "m" #'ess-noweb-mark-chunk
        ;; "p" #'ess-noweb-previous-chunk
        ;; "n" #'ess-noweb-next-chunk
        )
  )
