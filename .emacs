(require 'cl)

(defvar *emacs-load-time*
  ;; for recording load time
  (cdr (current-time)))

;;;;;;;;;;;;;;;;;;;;
;; customizations ;;
;;;;;;;;;;;;;;;;;;;;

;; custom-set-variables works better than setq and setq-default,
;; and custom-set-faces better than set-face-attributes;
;; But if you let Custom edit them, it will mess the comments.

(custom-set-variables
 ;; frame
 '(default-frame-alist (quote ((width . 150) (height . 45))))
 '(split-height-threshold 60)
 '(split-width-threshold 90)
 '(frame-title-format "%b (%f)" t) ;; full path in title bar
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(column-number-mode t)
 '(global-hl-line-mode t)
 '(ring-bell-function (quote ignore) t)
 ;; clipboard
 '(x-select-enable-clipboard t)
 '(save-interprogram-paste-before-kill t)
 ;; cursor
 '(blink-cursor-mode nil)
 '(cursor-type (quote (bar . 3)))
 '(cursor-in-non-selected-windows (quote hollow))
 ;; always uses spaces for tabs (for real tabs, use C-q)
 ;; see also tabify, untabify, and tab-width
 '(indent-tabs-mode nil)
 '(electric-indent-mode nil)
 ;; file
 '(create-lockfiles nil)
 '(auto-save-default nil)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(package-enable-at-startup nil))

(custom-set-faces
 '(region ((t (:background "black"))))
 '(cursor ((t (:background "goldenrod"))))
 '(mc/cursor-face ((t (:background "goldenrod" :foreground "black" :weight black))))
 '(default ((t (:height 140)))))

;; keys for switching windows
(windmove-default-keybindings 'meta)

;; enable some disabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; I have set package-enable-at-startup to nil
;; now we initialize packages but activate them not
(package-initialize nil)

;; use use-package package for managing packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
;; for checking when which package is loaded
;; (setq use-package-verbose t)

;; these two are needed for use-package
(use-package diminish)
(use-package bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; usage:						  ;;
;; see "C-h f use-package" for more			  ;;
;; (use-package package-name				  ;;
;; 							  ;;
;;   :disabled						  ;;
;;   ignores this altogether				  ;;
;; 							  ;;
;;   :if EXPR						  ;;
;;   ignores this if EXPR evals to nil			  ;;
;; 							  ;;
;;   :ensure package-name				  ;;
;;   installs package-name if missing			  ;;
;; 							  ;;
;;   :diminish mode-name &optional name-string		  ;;
;;   reduces mode-name to name-string in mode line	  ;;
;; 							  ;;
;;   :init &rest EXPRS					  ;;
;;   evals EXPRS before loading package-name		  ;;
;; 							  ;;
;;   :defer t						  ;;
;;   defers loading package-name			  ;;
;;   implied by :commands :bind :bind* :mode :interpreter ;;
;;   t can also be an integer,				  ;;
;;   to force loading after t seconds of idle time	  ;;
;; 							  ;;
;;   :commands (&rest commands)				  ;;
;;   defines autoloads for commands			  ;;
;; 							  ;;
;;   :mode ("extension" . mode-name)			  ;;
;;   adds mode-name to auto-mode-alist			  ;;
;;   takes multiple dotted pairs			  ;;
;; 							  ;;
;;   :interpreter ("extension" . interpreter-mode-alist)  ;;
;;   adds mode-name to interpreter-mode-alist		  ;;
;;   takes multiple dotted pairs			  ;;
;; 							  ;;
;;   :bind &rest ARGS					  ;;
;;   works like bind-keys, with autoloads		  ;;
;; 							  ;;
;;   :bind* &rest ARGS					  ;;
;;   works like bind-keys* with autoloads		  ;;
;; 							  ;;
;;   :demand						  ;;
;;   prevents deferred loading in all cases		  ;;
;; 							  ;;
;;   :config &rest EXPRS				  ;;
;;   evals EXPRS after loading package-name)		  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package benchmark-init
  ;; benchmarking require and load functions
  ;; for finding out where time is being spent
  ;; :disabled
  :config (benchmark-init/activate))

;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

(use-package exec-path-from-shell
  ;; sets environment variables correctly for OS X
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "LANG" "LC_ALL"))
  ;; fullscreen shortcut for mac, 's' is the right cmd key
  (bind-key "C-s-f" 'toggle-frame-fullscreen))

(use-package hc-zenburn-theme
  ;; custom color theme
  :config (load-theme 'hc-zenburn t))

(use-package magit
  ;; defer loading until the mode is called
  :commands (magit-status))

(use-package rainbow-mode
  ;; can be handy sometimes but rarely needed
  ;; defer loading until the mode is called
  :commands (rainbow-mode))

;;;;;;;;;;;;;;;
;; languages ;;
;;;;;;;;;;;;;;;

(use-package eldoc
  ;; defer until the mode is needed
  :defer t
  :diminish eldoc-mode "Eld"
  :init
  (add-hook 'clojure-mode-hook 'eldoc-mode 1)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode 1)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode 1)
  (add-hook 'ielm-mode-hook 'eldoc-mode 1)
  :config
  ;; since C-j is shadowed by smartparens
  (bind-keys :map lisp-mode-shared-map
	     ("<C-return>" . eval-last-sexp)
	     ("<M-return>" . eval-print-last-sexp)
	     ("<C-M-return>" . eval-defun)
	     ("<C-S-return>" . eval-region)
	     ("<M-S-return>" . eval-buffer)
	     ("<C-M-S-return>" . load-file)))

(use-package clojure-mode
  ;; defer until the mode is needed
  ;; see mode info: C-h v auto-mode-alist
  :mode
  ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode)
  ("\\.cljx\\'" . clojurex-mode)
  ("\\.cljc\\'" . clojurec-mode)
  ("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
  :config
  (use-package clojure-mode-extra-font-locking)

  (use-package clojure-cheatsheet
    :bind ("C-c C-h" . clojure-cheatsheet))

  (use-package cider
    :diminish cider-mode "cider"
    :config (setq cider-repl-history-file "~/.emacs.d/cider-history")
    (bind-keys :map cider-mode-map
	       ("<C-return>" . cider-eval-last-sexp)
	       ("<M-return>" . cider-eval-print-last-sexp)
	       ("<S-return>" . cider-eval-last-sexp-and-replace)
	       ("<C-M-return>" . cider-eval-defun-at-point)
	       ("<C-S-return>" . cider-eval-region)
	       ("<M-S-return>" . cider-eval-buffer)
	       ("<C-M-S-return>" . cider-load-file))

    (use-package ac-cider
      :config
      (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
      (add-hook 'cider-mode-hook 'ac-cider-setup)
      (eval-after-load "auto-complete"
	'(progn (add-to-list 'ac-modes 'cider-mode))))))

(use-package js2-mode
  ;; defer until the mode is needed
  :mode ("\\.js\\'" . js2-mode))

(use-package ess-site
  ;; defer until the main function is called
  :ensure ess
  :commands R)

(use-package tex
  ;; defer until the mode is needed
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
  
  (use-package latex-preview-pane)
  (use-package cdlatex)
  (use-package ac-math)

  (eval-after-load "auto-complete"
    '(progn (add-to-list 'ac-modes 'latex-mode)
            (use-package auto-complete-auctex)))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (TeX-PDF-mode 1) ;; ???
              ;; (flyspell-mode 1) ???
              (latex-preview-pane-enable)
              (push
               ;; notes on ~/.latexmkrc for SyncTeX setup
               ;; notes about skim preference settings
               '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                 :help "Run latexmk on file")
               TeX-command-list)
              (server-start)
              (setq TeX-command-default "latexmk")
              (turn-on-reftex)
              (LaTeX-math-mode 1)
              (turn-on-cdlatex)
              (setq ac-sources
                    (append '(ac-source-math-unicode
                              ac-source-math-latex
                              ac-source-latex-commands)
                            ac-sources))))

  (when (eq system-type 'darwin)
    (setq TeX-view-program-selection
          '((output-dvi "DVI Viewer")
            (output-pdf "PDF Viewer")
            (output-html "HTML Viewer")))
    (setq TeX-view-program-list
          '(("DVI Viewer" "open %o")
            ("PDF Viewer" "/opt/homebrew-cask/Caskroom/skim/1.4.14/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
            ("HTML Viewer" "open %o")))))

;;;;;;;;;;;;;;;;
;; navigation ;;
;;;;;;;;;;;;;;;;

(use-package ido
  ;; don't need ido if I just want to click open some file
  ;; slow to load, defer until main functions are called
  :bind
  ("C-x C-f" . ido-find-file) ;; C-f again for normal find
  ("C-x b" . ido-switch-buffer) 
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess)
  (use-package ido-complete-space-or-hyphen)
  
  (use-package ido-ubiquitous
    :config (ido-ubiquitous-mode 1))

  (use-package ido-vertical-mode
    :config (ido-vertical-mode 1)
    (setq ido-vertical-show-count t)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only))

  (use-package flx-ido
    :config (flx-ido-mode 1)
    (setq ido-use-faces nil) ;; meaning flx-ido-use-faces t
    (setq gc-cons-threshold 20000000))

  (use-package recentf
    :init (setq recentf-max-saved-items 30)
    :config (recentf-mode 1)))

(use-package smex
  ;; slow to load, mainly because smex has to load ido first
  ;; defer until main functions are called
  :bind ("M-x" . smex) ("M-X" . smex-major-mode-commands))

(use-package ace-jump-mode
  ;; defer until main functions are called
  :bind ("C-c j" . ace-jump-mode)
  ("C-x j" . ace-jump-mode-pop-mark)
  :config (ace-jump-mode-enable-mark-sync))

(use-package projectile
  ;; defer loading until the mode is called
  :bind ("C-c C-p" . projectile-mode)
  ("C-x p" . projectile-global-mode))

;;;;;;;;;;;;;
;; editing ;;
;;;;;;;;;;;;;

(use-package smartparens-config
  ;; defer loading til idle for one sec---although these editing aids
  ;; are surely needed if I'm doing editing---I may not do editing
  ;; if I just want to click open some file
  :ensure smartparens
  :defer 1
  :config
  (set-face-attribute 'sp-show-pair-match-face nil :weight 'black
		      :background "black" :foreground "firebrick")
  (set-face-attribute 'sp-show-pair-mismatch-face nil :weight 'black
		      :background "firebrick" :foreground "black")
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (diminish 'smartparens-mode "<>")
  ;; code by Rommel M. Martinez, see link below
  (defmacro def-pairs (pairs)
    `(progn
       ,@(loop for (key . val) in pairs
	       collect
	       `(defun ,(read (concat
			       "wrap-with-"
			       (prin1-to-string key)
			       "s"))
		    (&optional arg)
		  (interactive "p")
		  (sp-wrap-with-pair ,val)))))
  (def-pairs ((paren        . "(")
	      (bracket      . "[")
	      (brace        . "{")
	      (single-quote . "'")
	      (double-quote . "\"")
	      (back-quote   . "`")))
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
	     ("C-M-n" . sp-next-sexp)	      ;; forward-list
	     ("C-M-p" . sp-previous-sexp)     ;; backward-list
	     ("C-j" . sp-newline) ;; electric-newline-and-maybe-indent
	     ("C-M-t"   . sp-transpose-sexp)	    ;; transpose-sexp
	     ("C-x C-t" . sp-transpose-hybrid-sexp) ;; transpose-lines
	     ("C-M-k"           . sp-kill-sexp)	         ;; kill-sexp
	     ("<C-M-backspace>" . sp-backward-kill-sexp) ;; backward-kill-sexp
	     ;; strict mode stuff
	     ("C-d"   . sp-delete-char)          ;; delete-char
	     ("DEL"   . sp-backward-delete-char) ;; backward-delete-char
	     ("M-d"   . sp-kill-word)		 ;; kill-word
	     ("M-DEL" . sp-backward-kill-word)	 ;; backward-kill-word
	     ("C-k"   . sp-kill-hybrid-sexp)	 ;; kill-line
	     ;; https://ebzzry.github.io/emacs-pairs.html
	     ("C-c ("  . wrap-with-parens)
	     ("C-c ["  . wrap-with-brackets)
	     ("C-c {"  . wrap-with-braces)
	     ("C-c '"  . wrap-with-single-quotes)
	     ("C-c \"" . wrap-with-double-quotes)
	     ("C-c _"  . wrap-with-underscores)
	     ("C-c `"  . wrap-with-back-quotes))

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
    (bind-keys ("<C-tab>" . hippie-expand)
	       ("M-/" . crazy-hippie-expand)))

  (use-package region-bindings-mode
    ;; hide some more goodies here
    :config (region-bindings-mode-enable)
    (bind-keys :map region-bindings-mode-map
	       ("q" . keyboard-quit)          ;; quit
	       ("k" . kill-region)            ;; kill
	       ("z" . delete-region)          ;; zehen
	       ("c" . kill-ring-save)         ;; copy
	       ("r" . replace-string)         ;; replace
	       ("u" . upcase-initials-region) ;; upcase
	       ("w" . comment-box)            ;; wrap
	       ("g" . comment-or-uncomment-region) ;; gloss
	       ("n" . mc/mark-next-like-this)       ;; next
	       ("j" . mc/unmark-next-like-this)     ;; jerk
	       ("p" . mc/mark-previous-like-this)   ;; prev
	       ("o" . mc/unmark-previous-like-this) ;; off
               ("f" . mc/skip-to-next-like-this)     ;; forward
	       ("b" . mc/skip-to-previous-like-this) ;; backward
	       ("a" . mc/edit-beginnings-of-lines) ;; anfang
	       ("e" . mc/edit-ends-of-lines)       ;; ende
	       ("l" . mc/edit-lines)		   ;; line
	       ("x" . mc/mark-all-like-this-dwim)     ;; x
	       ("m" . mc/mark-all-in-region)	      ;; mark
	       ("t" . mc/mark-sgml-tag-pair)          ;; tag
	       ("d" . mc/mark-all-like-this-in-defun) ;; defun
	       ("h" . mc/mark-all-like-this)	      ;; (w)hole
	       ("i" . mc/insert-numbers)  ;; index
	       ("s" . mc/sort-regions)    ;; sort
	       ("v" . mc/reverse-regions) ;; vert
	       )
    (use-package multiple-cursors
      ;; defer until main functions are called from here
      ;; or from the region-bindings-mode-map
      :bind
      ("C-'"  . mc/mark-pop) ;; also runs mc-hide-unmatched-lines-mode
      ("C-\"" . mc/mark-all-dwim)
      ("<C-right>" . mc/mark-next-like-this)
      ("<C-up>"    . mc/mark-previous-like-this)
      ("<C-down>"  . mc/unmark-previous-like-this)
      ("<C-left>"  . mc/unmark-next-like-this))

    (use-package expand-region
      ;; defer until the main function is called
      :bind ("S-SPC" . er/expand-region))))

(use-package yasnippet
  ;; I use <M-tab> for yas and <C-tab> for hippie-expand
  ;; <tab> sometimes gets shadowed
  ;; defer loading until <M-tab> is calld
  :init (setq yas-snippet-dirs '(yas-installed-snippets-dir))
  :bind ("<M-tab>" . yas-global-mode)
  :config
  (diminish 'yas-minor-mode " Y")
  (unbind-key "<M-tab>" global-map)
  (bind-key "<M-tab>" 'yas-expand yas-minor-mode-map))

(use-package auto-complete-config
  ;; de/activate ac mode with <S-tab>
  ;; defer loading until <S-tab> is called
  :ensure auto-complete
  :bind ("<S-tab>" . global-auto-complete-mode)
  :config (ac-config-default))

(use-package rainbow-delimiters
  ;; lightweight and fun, why not
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

;;;;;;;;;;;;;;;;;;;;;;
;; custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; a very handy function from
;; https://github.com/flyingmachine/emacs-for-clojure
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
			       (line-end-position)))
(bind-key "C-;" 'toggle-comment-on-line)



(destructuring-bind (HIGH LOW USEC PSEC) (current-time)
  ;; loading finished
  (setq *emacs-load-time*
	(+ (- LOW (car *emacs-load-time*))
	   (/ (- USEC (cadr *emacs-load-time*)) 1000000.0)))
  (message ".emacs loaded in %fs" *emacs-load-time*))
