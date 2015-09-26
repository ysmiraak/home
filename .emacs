(require 'cl)
(defvar *emacs-load-time* (cdr (current-time)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(auto-save-default nil)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default)))
 '(default-frame-alist (quote ((width . 150) (height . 45))))
 '(electric-indent-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((top . 0) (left . 0) (width . 140) (height . 45))))
 '(package-enable-at-startup nil)
 '(save-interprogram-paste-before-kill t)
 '(scroll-bar-mode nil)
 '(sp-highlight-pair-overlay nil)
 '(split-height-threshold 60)
 '(split-width-threshold 90)
 '(tool-bar-mode nil)
 '(x-select-enable-clipboard t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#181A26" :foreground "gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(cursor ((t (:background "goldenrod"))))
 '(region ((t (:background "pale goldenrod" :foreground "black" :weight black))))
 '(sp-show-pair-match-face ((t (:background "pale goldenrod" :foreground "black" :weight black))))
 '(sp-show-pair-mismatch-face ((t (:background "#AA381E" :foreground "black" :weight black)))))

;;;;;;;;;;;;;;;;;;;;
;; customizations ;;
;;;;;;;;;;;;;;;;;;;;

(setq-default
 frame-title-format "%b (%f)" ;; full path in title bar
 cursor-type '(bar . 3) ;; I don't like my cursor fat
 cursor-in-non-selected-windows 'hollow)

(setq ring-bell-function 'ignore) ;; no bell

(windmove-default-keybindings 'meta) ;; keys for switching windows

;;;;;;;;;;;;;;
;; packages ;;
;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; I have set package-enable-at-startup to nil
;; now we initialize packages but activate them not
(package-initialize nil)

;; use use-package for managing packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; uncomment all lines with the word "ensure" for automatic installation
;; but of course not all of them
;; you know what I mean
;; hopefully
;; (setq use-package-always-ensure t)
(eval-when-compile (require 'use-package))
(use-package diminish)
(use-package bind-key)
;; for checking when which package is loaded
;; (setq use-package-verbose t)

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
;;   :diminish mode-name				  ;;
;;   hides mode-name in mode line			  ;;
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
;; 							  ;;
;;   :interpreter ("extension" . interpreter-mode-alist)  ;;
;;   adds mode-name to interpreter-mode-alist		  ;;
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

(use-package rainbow-mode
  ;; can be handy sometimes but rarely needed
  ;; defer loading until the mode is called
  :commands (rainbow-mode))

(use-package exec-path-from-shell
  ;; sets environment variables correctly for OS X
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "LANG" "LC_ALL"))
  ;; fullscreen shortcut for mac, 's' is the right cmd key
  (bind-key "C-s-f" 'toggle-frame-fullscreen))

;;;;;;;;;;;;;;;
;; languages ;;
;;;;;;;;;;;;;;;

(use-package eldoc
  ;; defer until the mode is needed
  :defer t :diminish eldoc-mode "EL"
  :init (add-hook 'clojure-mode-hook 'eldoc-mode 1)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode 1)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode 1)
  (add-hook 'ielm-mode-hook 'eldoc-mode 1)
  :config ;; since C-j is shadowed by smartparens
  (bind-key "RET" 'eval-print-last-sexp lisp-interaction-mode-map))

(use-package clojure-mode
  ;; defer until the mode is needed
  :mode
  ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode)
  ("\\.cljx\\'" . clojurex-mode)
  ("\\.cljc\\'" . mode)
  ("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
  :config
  (use-package clojure-mode-extra-font-locking)
  (use-package cider :diminish cider-mode "cider"
    :config (setq cider-repl-history-file "~/.emacs.d/cider-history")
    (use-package ac-cider :defer t
      :config
      (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
      (add-hook 'cider-mode-hook 'ac-cider-setup)
      (eval-after-load "auto-complete"
	'(progn (add-to-list 'ac-modes 'cider-mode)))))
  (use-package clojure-cheatsheet
    :bind ("C-c C-h" . clojure-cheatsheet)))

(use-package js2-mode
  ;; defer until the mode is needed
  :mode ("\\.js\\'" . js-mode))

;;;;;;;;;;;;;;;;
;; navigation ;;
;;;;;;;;;;;;;;;;

(use-package ido
  ;; don't need ido if I just want to click open some file
  ;; slow to load, defer until main functions are called
  :bind
  ("C-x C-f" . ido-find-file)
  ("C-x b" . ido-switch-buffer) 
  :config
  (ido-mode 1)
  (setq ido-use-faces t
	ido-enable-flex-matching t
	ido-use-filename-at-point nil
	ido-use-virtual-buffers 'auto)
  (use-package ido-ubiquitous
    :config (ido-ubiquitous-mode 1))
  (use-package ido-vertical-mode
    :config (ido-vertical-mode 1)
    (setq ido-vertical-show-count t)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)
    (set-face-attribute
     'ido-vertical-first-match-face nil
     :background "light goldenrod" :foreground "#181A26")
    (set-face-attribute
     'ido-vertical-only-match-face nil
     :background nil :foreground "goldenrod")
    (set-face-attribute
     'ido-vertical-match-face nil
     :background nil :foreground "#AA381E"))
  (use-package ido-complete-space-or-hyphen)
  (use-package recentf
    :init (setq recentf-max-saved-items 30)
    :config (recentf-mode 1)))

(use-package smex
  ;; slow to load, mainly because smex has to load ido first
  ;; defer until main functions are called
  :bind ("M-x" . smex) ("M-X" . smex-major-mode-commands))

;;;;;;;;;;;;;
;; editing ;;
;;;;;;;;;;;;;

;; It's a mode for abbreviations, right?
;; not a package, cannot use use-packge for this
(eval-after-load "abbrev" '(diminish 'abbrev-mode "."))

(use-package smartparens-config

  ;; some reason for the defer
  
  ;; :ensure smartparens
  :defer 1
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-highlight-pair-overlay nil)
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
	     ("M-s" . sp-splice-sexp)
	     ("M-S" . sp-split-sexp)
	     ("M-J" . sp-join-sexp)
	     ("M-C" . sp-convolute-sexp) ;; formerly M-?
	     ;; more magic added by smartparens
	     ("M-A" . sp-absorb-sexp)
	     ("M-E" . sp-emit-sexp)
	     ("M-I" . sp-indent-defun)
	     ("M-R" . sp-rewrap-sexp)
	     ("M-W" . sp-swap-enclosing-sexp)
	     ("M-[" . sp-select-previous-thing-exchange)
	     ("M-]" . sp-select-next-thing)
	     ("M-(" . sp-extract-before-sexp)
	     ("M-)" . sp-extract-after-sexp)
	     ("C-<" . sp-indent-adjust-sexp) ;; cp. sp-add-to-previous-sexp
	     ("C->" . sp-add-to-next-sexp)
	     ("C-M-w" . sp-copy-sexp) ;; C-- for backward copy
	     ;; same as paredit/sp-raise-sexp, used to be bound to M-r
	     ;; default behavior: sp-splice-sexp-killing-backward
	     ;; C-- for sp-splice-sexp-killing-forward
	     ;; overrides backward-up-list, raise up!
	     ("C-M-u" . sp-splice-sexp-killing-around)
	     ;; navigation via parentheses
	     ("C-S-f" . sp-down-sexp)
	     ("C-S-b" . sp-backward-down-sexp)
	     ("C-S-a" . sp-backward-up-sexp)
	     ("C-S-e" . sp-up-sexp)
	     ("C-S-p" . sp-end-of-previous-sexp) ;; C-- for next
	     ("C-S-n" . sp-beginning-of-next-sexp) ;; C-- for previous
	     ;; emacs stuff ;; overrides
	     ("C-M-f" . sp-forward-sexp) ;; forward-sexp
	     ("C-M-b" . sp-backward-sexp)
	     ("C-M-a" . sp-beginning-of-sexp) ;; beginning-of-defun
	     ("C-M-e" . sp-end-of-sexp)
	     ("C-M-n" . sp-next-sexp) ;; forward-list
	     ("C-M-p" . sp-previous-sexp)
	     ("C-j" . sp-newline)	    ;; electric-newline-and-maybe-indent
	     ("C-M-t"   . sp-transpose-sexp)	       ;; transpose-sexp
	     ("C-x C-t" . sp-transpose-hybrid-sexp) ;; transpose-lines
	     ("<C-backspace>" . sp-backward-unwrap-sexp)
	     ("C-M-d" . sp-unwrap-sexp) ;; down-list
	     ("C-M-k"           . sp-kill-sexp)	  ;; kill-sexp
	     ("<C-M-backspace>" . sp-backward-kill-sexp)
	     ;; strict mode stuff
	     ("C-d"   . sp-delete-char) ;; delete-char
	     ("DEL"   . sp-backward-delete-char)
	     ("M-d"   . sp-kill-word) ;; kill-word
	     ("M-DEL" . sp-backward-kill-word)
	     ("C-k"   . sp-kill-hybrid-sexp) ;; kill-line
	     ;; for symbols, analogous to meta cords for words
	     ("M-F" . sp-forward-symbol)
	     ("M-B" . sp-backward-symbol)
	     ("M-D" . sp-kill-symbol)
	     ("<M-S-backspace>" . sp-backward-kill-symbol)
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
    (bind-keys ("<tab>" . hippie-expand)
	       ;; sometimes tab gets shadowed
	       ("<M-tab>" . hippie-expand)
	       ("M-/" . crazy-hippie-expand))))

(use-package yasnippet
  ;; I use <C-tab> for yas and <tab> for hippie-expand
  ;; cuz I use the later more often
  ;; defer loading until <C-tab> is calld
  :init (setq yas-snippet-dirs '(yas-installed-snippets-dir))
  :bind ("<C-tab>" . yas-global-mode)
  :config (diminish 'yas-minor-mode " Y")
  (unbind-key "<C-tab>" global-map)
  (bind-key "<C-tab>" 'yas-expand yas-minor-mode-map))

(use-package auto-complete-config
  ;; de/activate ac mode with <S-tab>
  ;; when ac pops up, use <S-tab> for selection
  ;; defer loading until <S-tab> is called
  ;; :ensure auto-complete
  :bind ("<S-tab>" . global-auto-complete-mode)
  :config (ac-config-default)
  (bind-key "<S-tab>" 'ac-next ac-mode-map))

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
  (setq *emacs-load-time*
	(+ (- LOW (car *emacs-load-time*))
	   (/ (- USEC (cadr *emacs-load-time*)) 1000000.0)))
  (message ".emacs loaded in %fs" *emacs-load-time*))
