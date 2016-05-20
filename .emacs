;;;;;;;;;;;;;;;;;;;;
;; customizations ;;
;;;;;;;;;;;;;;;;;;;;

;; custom-set-variables and custom-set-face work better
;; than setq/setq-default and set-face-attributes;
;; But if you let Custom edit them, it will mess up the comments.

(custom-set-variables
 ;; The Scarab: City-Face
 ;; '(default-frame-alist '((width . 150) (height . 45)))
 ;; '(split-height-threshold 60)
 ;; '(split-width-threshold 90)
 '(frame-title-format "%b [%f]" t)
 '(uniquify-buffer-name-style 'forward)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(column-number-mode t)
 '(ring-bell-function 'ignore t)
 ;; clipboard
 '(x-select-enable-clipboard t)
 '(save-interprogram-paste-before-kill t)
 ;; cursor
 '(blink-cursor-mode nil)
 '(cursor-type '(bar . 3))
 '(cursor-in-non-selected-windows 'hollow)
 ;; always uses spaces for tabs (for real tabs, use C-q)
 ;; see also tabify, untabify, and tab-width
 '(indent-tabs-mode nil)
 '(electric-indent-mode nil)
 ;; file
 '(create-lockfiles nil)
 '(auto-save-default nil)
 '(version-control t)
 '(delete-old-versions t)
 '(backup-directory-alist `(("." . ,temporary-file-directory)))
 '(package-enable-at-startup nil))

(custom-set-faces
 ;; I bow not yet before the Iron Crown,
 '(region ((t (:background "#242424")))) ;; Nirn gray
 ;; nor cast my own small golden sceptre down.
 '(cursor ((t (:background "#DAA520")))) ;; goldenrod
 ;; Ink and gold.
 '(mc/cursor-face ((t (:background "#DAA520" :foreground "#242424")))))

;; keys for switching windows
(windmove-default-keybindings 'meta)

;; enable some disabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; I have set package-enable-at-startup to nil
;; now we initialize packages without activation
(package-initialize nil)

;; use use-package package for managing packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
;; (setq use-package-always-ensure t)
;; (setq use-package-verbose t)

;; these two are needed for use-package
(use-package diminish)
(use-package bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; usage:                                                 ;;
;; see "C-h f use-package" for more                       ;;
;; (use-package package-name                              ;;
;;                                                        ;;
;;   :disabled                                            ;;
;;   ignores this altogether                              ;;
;;                                                        ;;
;;   :if EXPR                                             ;;
;;   ignores this if EXPR evals to nil                    ;;
;;                                                        ;;
;;   :ensure package-name                                 ;;
;;   installs package-name if missing                     ;;
;;                                                        ;;
;;   :diminish (mode-name . name-string)                  ;;
;;   reduces mode name to name-string in mode line        ;;
;;                                                        ;;
;;   :init &rest EXPRS                                    ;;
;;   evals EXPRS before loading package-name              ;;
;;                                                        ;;
;;   :defer                                               ;;
;;   defers loading package-name                          ;;
;;   implied by :commands :bind :bind* :mode :interpreter ;;
;;   t can also be an integer,                            ;;
;;   to force loading after t seconds of idle time        ;;
;;                                                        ;;
;;   :commands (&rest commands)                           ;;
;;   defines autoloads for commands                       ;;
;;                                                        ;;
;;   :mode ("extension" . mode-name)                      ;;
;;   adds mode-name to auto-mode-alist                    ;;
;;   takes multiple dotted pairs                          ;;
;;                                                        ;;
;;   :interpreter ("extension" . interpreter-mode-alist)  ;;
;;   adds mode-name to interpreter-mode-alist             ;;
;;   takes multiple dotted pairs                          ;;
;;                                                        ;;
;;   :bind &rest ARGS                                     ;;
;;   works like bind-keys, with autoloads                 ;;
;;                                                        ;;
;;   :bind* &rest ARGS                                    ;;
;;   works like bind-keys* with autoloads                 ;;
;;                                                        ;;
;;   :demand                                              ;;
;;   prevents deferred loading in all cases               ;;
;;                                                        ;;
;;   :config &rest EXPRS                                  ;;
;;   evals EXPRS after loading package-name)              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package benchmark-init
  ;; benchmarking require and load functions
  ;; for finding out where time is being spent
  :disabled
  :config (benchmark-init/activate))

;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

(use-package exec-path-from-shell
  ;; sets environment variables correctly for OS X
  :if (memq window-system '(mac ns))
  :demand
  ;; fullscreen shortcut for mac, 's' is the cmd key
  :bind (("<C-s-f>" . toggle-frame-fullscreen)
         ("<C-s-268632070>" . toggle-frame-fullscreen))
  :config
  (toggle-frame-fullscreen)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "LANG" "LC_ALL" "EMAIL")))

(use-package zenburn-theme
  :config (load-theme 'zenburn t)
  (use-package hl-line
    :config (global-hl-line-mode 1))
  (use-package powerline
    :config (powerline-center-theme))
  (use-package rainbow-delimiters
    :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable)))

(use-package magit
  :commands magit-status)

(use-package rainbow-mode
  :commands rainbow-mode)

;;;;;;;;;;;;;;;
;; languages ;;
;;;;;;;;;;;;;;;

(use-package eldoc
  :bind ;; since C-j is shadowed by smartparens
  (:map lisp-mode-shared-map
        ("<C-return>" . eval-last-sexp)
        ("<M-return>" . eval-print-last-sexp)
        ("<C-M-return>" . eval-defun)
        ("<C-S-return>" . eval-region)
        ("<M-S-return>" . eval-buffer)
        ("<C-M-S-return>" . load-file))
  :diminish "Eld"
  :init
  (add-hook 'clojure-mode-hook 'eldoc-mode 1)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode 1)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode 1)
  (add-hook 'ielm-mode-hook 'eldoc-mode 1))

(use-package clojure-mode
  ;; see mode info: C-h v auto-mode-alist
  :mode
  ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode)
  ("\\.cljx\\'" . clojurex-mode)
  ("\\.cljc\\'" . clojurec-mode)
  ("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
  :config
  (use-package clojure-mode-extra-font-locking)

  (use-package clojure-cheatsheet :defer)

  (with-eval-after-load 'flycheck
    (use-package flycheck-clojure
      :config (flycheck-clojure-setup)))

  (use-package cider
    :diminish "cider"
    :config
    (setq cider-repl-display-help-banner nil
          cider-repl-history-file "~/.emacs.d/cider-history")
    :bind (:map cider-mode-map
                ("<C-return>" . cider-eval-last-sexp)
                ("<M-return>" . cider-eval-print-last-sexp)
                ("<S-return>" . cider-eval-last-sexp-and-replace)
                ("<C-M-return>" . cider-eval-defun-at-point)
                ("<C-S-return>" . cider-eval-region)
                ("<M-S-return>" . cider-eval-buffer)
                ("<C-M-S-return>" . cider-load-file))))

(with-eval-after-load 'prolog
  (use-package ediprolog
    ;; defer with :bind somehow does not work properly for ediprolog
    :config (bind-key "<C-return>" 'ediprolog-dwim prolog-mode-map)))
(setq auto-mode-alist
      (append '(("\\.pl$" . prolog-mode) ("\\.m$" . mercury-mode))
              auto-mode-alist))

(use-package python
  :bind (:map python-mode-map
              ("<C-M-return>" . python-shell-send-defun)
              ("<C-S-return>" . python-shell-send-region)
              ("<M-S-return>" . python-shell-send-buffer)
              ("<C-M-S-return>" . python-shell-send-file))
  :config (setq python-shell-interpreter "python3"))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

(use-package ess-site
  :ensure ess
  :commands R
  :bind (:map ess-mode-map
              ("<C-return>" . ess-eval-line)
              ("<C-M-return>" . ess-eval-function-or-paragraph)
              ("<C-S-return>" . ess-eval-region)
              ("<M-S-return>" . ess-eval-buffer)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-log-done 'time
        org-src-fontify-natively t
        org-latex-listings 'minted
        org-confirm-babel-evaluate nil)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  
  (use-package htmlize))

(use-package markdown-mode
  :mode
  ("\\.text\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.[rR]md" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  :config
  (setq markdown-enable-math t)
  (add-hook 'markdown-mode-hook 'visual-line-mode)

  (defun rmarkdown-new-chunk (&optional name)
    "Insert a new R chunk. https://gist.github.com/chlalanne/7403341"
    (interactive "sChunk name: ")
    (insert "\n```{r " name "}\n")
    (save-excursion
      (newline)
      (insert "```\n")
      (previous-line)))

  (defun rmarkdown-render-current-file-then-display (&optional EXTENSION)
    "Output format should be specified accordingly in YAML."
    (interactive "sOutput filename extension (default pdf): ")
    (shell-command
     (format "Rscript -e 'rmarkdown::render(\"%s\")'; and open %s"
             (shell-quote-argument (buffer-file-name))
             (shell-quote-argument
              (concat (file-name-sans-extension (buffer-file-name)) "."
                      (if (zerop (length EXTENSION)) "pdf" EXTENSION))))))

  (use-package markdown-mode+))

(use-package tex
  :ensure auctex
  :mode
  ("\\.hva\\'" . latex-mode)
  ("\\.drv\\'" . latex-mode)
  ("\\.[tT]e[xX]\\'" . tex-mode)
  ("\\.ins\\'" . tex-mode)
  ("\\.ltx\\'" . latex-mode)
  ("\\.dtx\\'" . doctex-mode)
  ("\\.sty\\'" . latex-mode)
  ("\\.cl[so]\\'" . latex-mode)
  ("\\.bbl\\'" . latex-mode)
  ("\\.bib\\'" . bibtex-mode)
  ("\\.bst\\'" . bibtex-style-mode)
  :config
  (setq-default TeX-master nil)
  (setq TeX-auto-save t
        TeX-parse-self t
        reftex-plug-into-AUCTeX t)

  (with-eval-after-load 'company
    (use-package company-auctex
      :config
      (company-auctex-init)
      (yas-minor-mode-on)
      (yas-reload-all)))

  (use-package cdlatex)

  (use-package latex-preview-pane)

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (flyspell-mode 1)
              (LaTeX-math-mode 1)
              (turn-on-reftex)
              (turn-on-cdlatex)
              (latex-preview-pane-enable)
              (TeX-PDF-mode 1)
              (push '("latexmk" "latexmk -latexoption=-shell-escape -pdf %s"
                      TeX-run-TeX nil t :help "Run latexmk on file")
                    TeX-command-list)
              (setq TeX-command-default "latexmk")
              (server-start)))

  (when (eq system-type 'darwin)
    (setq TeX-view-program-selection
          '((output-dvi "DVI Viewer")
            (output-pdf "PDF Viewer")
            (output-html "HTML Viewer")))
    (setq TeX-view-program-list
          '(("DVI Viewer" "open %o")
            ;; http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
            ("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
            ("HTML Viewer" "open %o")))))

;;;;;;;;;;;;;;;;
;; navigation ;;
;;;;;;;;;;;;;;;;

(use-package ido
  :init
  (add-hook 'window-setup-hook 'ido-mode) ;; defer til the end of start-up
  (setq ad-redefinition-action 'accept)
  :config
  (setq ad-redefinition-action 'warn)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t
        ido-max-work-directory-list 0
        ido-enable-last-directory-history nil)

  (use-package ido-complete-space-or-hyphen)

  (use-package ido-ubiquitous
    :config (ido-ubiquitous-mode 1))

  (use-package ido-vertical-mode
    :config (ido-vertical-mode 1)
    (setq ido-vertical-show-count t)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

  (use-package flx-ido
    :config (flx-ido-mode 1)
    (setq ido-use-faces nil
          gc-cons-threshold 20000000)))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package centered-cursor-mode
  :demand
  :bind ("C-x c" . global-centered-cursor-mode)
  :config (global-centered-cursor-mode 1))

(use-package avy
  :bind (("C-c j" . avy-goto-word-1) ;; same as ace-jump
         ("C-c l" . avy-goto-char)   ;; letter
         ("C-c k" . avy-goto-char-2) ;; keys
         ("M-g g" . avy-goto-line))
  :config (avy-setup-default))

(use-package projectile
  :bind ("C-x p" . projectile-global-mode))

;;;;;;;;;;;;;
;; editing ;;
;;;;;;;;;;;;;

(use-package expand-region
  :bind ("S-SPC" . er/expand-region))

(use-package multiple-cursors
  :bind
  (("C-'"  . mc/mark-pop) ;; also runs mc-hide-unmatched-lines-mode
   ("C-\"" . mc/mark-all-dwim)
   ("<C-right>" . mc/mark-next-like-this)
   ("<C-up>"    . mc/mark-previous-like-this)
   ("<C-down>"  . mc/unmark-previous-like-this)
   ("<C-left>"  . mc/unmark-next-like-this)))

(use-package smartparens-config
  ;; defer loading til idle for one sec---although these editing aids
  ;; are surely needed if I'm doing editing---I may not do editing
  ;; if I just want to click open some file
  :ensure smartparens
  :defer 1
  :diminish (smartparens-mode . "<>") ;; red diamond
  :config
  (set-face-attribute 'sp-show-pair-match-face nil ;; ELEGENT WEAPONS
                      :background "#181818"        ;; Star Wound
                      :foreground "#A41210" ;; Tamriel-Aetherius-Oblivion
                      :weight 'bold)
  (set-face-attribute 'sp-show-pair-mismatch-face nil ;; FOR A MORE... CIVILIZED AGE.
                      :background "#161616" ;; the void unknown
                      :foreground "#003B6F" ;; Tardis blue, Mnemoli
                      :weight 'black)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)

  (use-package hippie-exp
    ;; this package always gets loaded at startup even with defer
    ;; had to hide it here
    :config
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-expand-all-abbrevs))
    (fset 'crazy-hippie-expand
          (make-hippie-expand-function
           '(try-complete-file-name-partially
             try-complete-file-name
             try-expand-list
             try-expand-list-all-buffers
             try-expand-line
             try-expand-line-all-buffers
             try-expand-whole-kill) t))
    :bind (("<M-tab>" . hippie-expand)
           ("M-/" . crazy-hippie-expand)))

  (use-package undo-tree
    :diminish " ψ"
    :config (global-undo-tree-mode 1))

  (use-package region-bindings-mode
    :config (region-bindings-mode-enable)
    (bind-keys :map region-bindings-mode-map
               ("q" . keyboard-quit)                  ;; quit
               ("k" . kill-region)                    ;; kill
               ("z" . delete-region)                  ;; zehen
               ("c" . kill-ring-save)                 ;; copy
               ("r" . replace-string)                 ;; replace
               ("u" . upcase-initials-region)         ;; upcase
               ("w" . comment-box)                    ;; wrap
               ("g" . comment-or-uncomment-region)    ;; gloss
               ("n" . mc/mark-next-like-this)         ;; next
               ("j" . mc/unmark-next-like-this)       ;; jerk
               ("p" . mc/mark-previous-like-this)     ;; prev
               ("o" . mc/unmark-previous-like-this)   ;; off
               ("f" . mc/skip-to-next-like-this)      ;; forward
               ("b" . mc/skip-to-previous-like-this)  ;; backward
               ("a" . mc/edit-beginnings-of-lines)    ;; anfang
               ("e" . mc/edit-ends-of-lines)          ;; ende
               ("l" . mc/edit-lines)                  ;; line
               ("m" . mc/mark-all-like-this-dwim)     ;; mark
               ("x" . mc/mark-all-in-region)          ;; x
               ("t" . mc/mark-sgml-tag-pair)          ;; tag
               ("d" . mc/mark-all-like-this-in-defun) ;; defun
               ("h" . mc/mark-all-like-this)          ;; (w)hole
               ("i" . mc/insert-numbers)              ;; index
               ("s" . mc/sort-regions)                ;; sort
               ("v" . mc/reverse-regions)             ;; vert
               ))
  (bind-keys :map smartparens-mode-map
             ;; paredit stuff
             ("C-)" . sp-forward-slurp-sexp)
             ("C-}" . sp-dedent-adjust-sexp) ;; cp. sp-forward-barf-sexp
             ("C-(" . sp-backward-slurp-sexp)
             ("C-{" . sp-backward-barf-sexp)
             ("<C-backspace>" . sp-splice-sexp-killing-backward)
             ("C-M-d" . sp-splice-sexp-killing-forward) ;; overrides down-list
             ("M-s" . sp-splice-sexp)
             ;; <---??? maybe reconsider these key cords
             ("M-S" . sp-split-sexp)
             ("M-J" . sp-join-sexp)
             ("M-C" . sp-convolute-sexp) ;; formerly M-?
             ;; more magic added by smartparens
             ("M-A" . sp-absorb-sexp)
             ("M-E" . sp-emit-sexp)
             ("M-I" . sp-indent-defun)
             ("M-R" . sp-rewrap-sexp)
             ("M-W" . sp-swap-enclosing-sexp)
             ("M-(" . sp-extract-before-sexp)
             ("M-)" . sp-extract-after-sexp)
             ("C-<" . sp-indent-adjust-sexp) ;; cp. sp-add-to-previous-sexp
             ("C->" . sp-add-to-next-sexp)
             ;; they are kinda hard to remember ???--->
             ("M-[" . sp-select-previous-thing-exchange)
             ("M-]" . sp-select-next-thing)
             ("C-M-w" . sp-copy-sexp)
             ("C-M-S-w" . sp-backward-copy-sexp)
             ("C-M-u" . sp-unwrap-sexp) ;; overrides backward-up-list
             ("C-M-S-u" . sp-backward-unwrap-sexp)
             ;; navigation via parentheses
             ("M-n" . sp-down-sexp)
             ("M-P" . sp-backward-down-sexp)
             ("M-p" . sp-backward-up-sexp)
             ("M-N" . sp-up-sexp)
             ;; emacs stuff ;; overrides
             ("C-M-f" . sp-forward-sexp)      ;; forward-sexp
             ("C-M-b" . sp-backward-sexp)     ;; backward-sexp
             ("C-M-a" . sp-beginning-of-sexp) ;; beginning-of-defun
             ("C-M-e" . sp-end-of-sexp)       ;; end-of-defun
             ("C-M-n" . sp-next-sexp)         ;; forward-list
             ("C-M-p" . sp-previous-sexp)     ;; backward-list
             ("C-j" . sp-newline) ;; electric-newline-and-maybe-indent
             ("C-M-t"   . sp-transpose-sexp)        ;; transpose-sexp
             ("C-x C-t" . sp-transpose-hybrid-sexp) ;; transpose-lines
             ("C-M-k"           . sp-kill-sexp)     ;; kill-sexp
             ("<C-M-backspace>" . sp-backward-kill-sexp) ;; backward-kill-sexp
             ;; strict mode stuff
             ("C-d"   . sp-delete-char)          ;; delete-char
             ("DEL"   . sp-backward-delete-char) ;; backward-delete-char
             ("M-d"   . sp-kill-word)            ;; kill-word
             ("M-DEL" . sp-backward-kill-word)   ;; backward-kill-word
             ("C-k"   . sp-kill-hybrid-sexp)     ;; kill-line
             ))

(use-package yasnippet
  :bind (("<C-S-tab>" . yas-global-mode)
         :map yas-minor-mode-map
         ("<S-tab>" . yas-expand))
  :diminish (yas-minor-mode . " Y")
  :init (setq yas-snippet-dirs '(yas-installed-snippets-dir))
  :config
  (unbind-key "<tab>" yas-minor-mode-map)
  (unbind-key "TAB" yas-minor-mode-map))

(use-package company
  :bind ("<C-tab>" . global-company-mode)
  :diminish " K"
  :config
  (unbind-key "<tab>" company-active-map)
  (unbind-key "TAB" company-active-map)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t)

  (use-package company-math
    :config (add-to-list 'company-backends 'company-math-symbols-unicode))

  (use-package company-quickhelp
    :config (company-quickhelp-mode 1))

  (use-package company-flx
    :bind ("<C-M-tab>" . company-flx-mode)))

(use-package flycheck
  :bind ("<C-M-S-tab>" . global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :config
    (setq flycheck-display-errors-function
          #'flycheck-pos-tip-error-messages)))

(use-package flyspell
  :diminish " $"
  :bind ("C-$" . flyspell-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/rdallasgray/graphene/blob/master/graphene-helper-functions.el

(defun insert-semicolon-at-end-of-line ()
  "Add a closing semicolon from anywhere in the line."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun comment-current-line-dwim ()
  "Comment or uncomment the current line."
  (interactive)
  (save-excursion
    (push-mark (beginning-of-line) t t)
    (end-of-line)
    (comment-dwim nil)))

(bind-keys ("C-;" . insert-semicolon-at-end-of-line)
           ("C-M-;" . comment-current-line-dwim))

(defun increase-window-height (&optional arg)
  "Make the window taller by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg))

(defun decrease-window-height (&optional arg)
  "Make the window shorter by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg)))

(defun decrease-window-width (&optional arg)
  "Make the window narrower by one column. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg) t))

(defun increase-window-width (&optional arg)
  "Make the window wider by one column. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg t))

(bind-keys ("<S-up>" . increase-window-height)
           ("<S-down>" . decrease-window-height)
           ("<S-right>" . increase-window-width)
           ("<S-left>" . decrease-window-width))

(add-hook 'after-init-hook
          (lambda ()
            (setq initial-scratch-message
                  (concat initial-scratch-message
                          (format ";; Emacs initialized in %.2f seconds.\n\n"
                                  (float-time (time-subtract after-init-time before-init-time)))))))
