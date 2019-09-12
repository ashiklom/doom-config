;;; lang/ess/config.el -*- lexical-binding: t; -*-

(use-package! ess
  :commands (stata SAS)
  :mode (("\\.Rprofile\\'" . ess-r-mode)
         ("\\.[Rr]\\'" . ess-r-mode))
  :init
  (unless (featurep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode)))
  :config
  ;; Smartparens mode conflicts with electric pair mode
  (add-hook 'ess-mode-hook 'electric-pair-mode)
  (add-hook 'ess-mode-hook 'turn-off-smartparens-mode)
  ;; Common ESS settings
  (setq ess-eval-visibly 'nowait
        ess-fill-calls-newlines t
        ess-nuke-trailing-whitespace-p t
        ess-use-flymake (not (featurep! :tools flycheck))
        comint-move-point-for-output t)
  (defun ans/r-mode-settings ()
    "Custom R mode configurations."
    (setq ess-offset-continued 'straight
          ess-ask-for-ess-directory nil
          ess-directory-function #'ans-r-file-here
          ess-style 'RStudio
          ess-history-directory (expand-file-name "ess-history/" doom-cache-dir)
          ess-roxy-str "#'"
          ess-roxy-fill-param-p t
          ess-roxy-template-alist
          '(("description" . ".. title/description ..")
            ("param" . "")
            ("return" . "")
            ("author" . "Alexey Shiklomanov"))
          inferior-R-args "--no-save --no-restore")
    (setf (alist-get 'ess-fl-keyword:fun-calls ess-R-font-lock-keywords) t))
  (add-hook 'ess-mode-hook #'ans/r-mode-settings)
  ;; Allow this to be set via dir-locals without complaint
  (put 'ess-r-package-dirs 'safe-local-variable #'listp)
  (set-popup-rule!
    (rx string-start "*" (or "R" "julia") (any "*" ":"))
    ;; :quit #'ans/ess-close-if-not-running
    :quit nil
    :size 0.3
    :slot 2
    :vslot 2
    :select nil)

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

        (:map ess-mode-map
          (:localleader
            :n "rq" #'ess-quit))

        (:map ess-julia-mode-map
          (:localleader
            "rf" #'julia
            [tab]     #'ess-switch-to-inferior-or-script-buffer
            [backtab] #'ess-switch-process
            ;; REPL
            "," #'ess-eval-region-or-function-or-paragraph-and-step
            "B" #'ess-eval-buffer-and-go
            "b" #'ess-eval-buffer
            "d" #'ess-eval-region-or-line-and-step
            "D" #'ess-eval-function-or-paragraph-and-step
            "L" #'ess-eval-line-and-go
            "l" #'ess-eval-line
            "R" #'ess-eval-region-and-go
            "F" #'ess-eval-function-and-go
            "f" #'ess-eval-function
            "pp" #'ess-eval-paragraph
            "pd" #'ess-eval-paragraph-and-go
            :v "ss" #'ess-eval-region))
       
        (:map ess-r-mode-map
          (:localleader
            :n "r f" #'ess-switch-process
            ;; Built-in versions of these commands
            ;; :n "l" #'ess-eval-line
            ;; :n "d" #'ess-eval-line-and-step
            ;; :n "ff" #'ess-eval-function-or-paragraph
            ;; :n "pp" #'ess-eval-paragraph
            ;; :n "pd" #'ess-eval-paragraph-and-step
            ;; :n "aa" #'ess-eval-buffer
            :n "l" #'ans/r-send-line
            :n "d" #'ans/r-send-line-and-down
            :n "f f" #'ans/r-send-function-or-paragraph
            :n "p p" #'ans/r-send-paragraph
            :n "p d" #'ans/r-send-paragraph-and-down
            :n "p l" #'ans/ess-eval-pipe-through-line
            :n "a a" #'ans/r-send-buffer
            :n "a d" #'ans/r-send-current-line-to-end
            :n "a s" #'ans/r-send-beginning-to-current-line
            :n "r p" #'ans/ess-eval-symbol
            :n "r g" #'ans/ess-glimpse-symbol
            :n "r s" #'ans/ess-str
            :n "r z" #'ans/ess-symbol-size
            :n "r n" #'ans/ess-names
            :n "r H" #'ans/ess-head
            :n "r T" #'ans/ess-tail
            :n "k r" #'ans/rmarkdown-render
            :n "X " #'ess-interrupt
            :n "0" #'ans/ess-dev-off
            :n "!" #'ans/ess-toggle-debug
            :n "z p" #'ans/ess-usethis-package
            :n "r l" #'ans/ess-drake-loadd
            :n "r d" #'ans/ess-drake-readd
            :n "r b" #'ans/ess-drake-build-target
            :n "r i" (lambda (arg) (interactive "P") (ans/import/add-to-imports arg) (ans/import/source-imports))
            :n "," #'ans/split-path-string
            :n "$" #'ans/ess-dollar-to-bracket
            :n "<backspace>" #'ans/ess-rm
            :n "b t" #'ans/ess-trace-back
            :n "b T" #'ans/ess-last-error
            :n "h" 'ess-doc-map
            :n "x" 'ess-extra-map
            :n "v" 'ess-r-package-dev-map
            :n "e" 'ess-dev-map
            :v "s s" #'ans/r-send-region-source
            :v "s i" #'ans/ess-plot-imgur
            :v "r x" #'ans/ess-reprex-region
            :v "=" #'ans/ess-tidy-region)
          :i "_" #'self-insert-command
          :i "M--" #'ess-insert-assign
          :i "M-m" (lambda() (interactive)(insert " %>%"))
          :i "C-'" (lambda() (interactive) (insert "#'")))

        ;; Old bindings
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
