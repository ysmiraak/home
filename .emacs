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
  :bind ("C-c m m" . magit-status))

(use-package rainbow-mode
  :bind ("C-c m r" . rainbow-mode))

;;;;;;;;;;;;;;;
;; languages ;;
;;;;;;;;;;;;;;;

(use-package eldoc
  :diminish "Eld"
  :bind (:map lisp-mode-shared-map
              ("<C-return>" . eval-last-sexp)
              ("<M-return>" . eval-defun)
              ("<S-return>" . eval-region)
              ("<C-M-return>" . eval-buffer)
              ("<C-S-return>" . eval-print-last-sexp))
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode 1)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode 1)
  (add-hook 'clojure-mode-hook 'eldoc-mode 1)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode 1)
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
    :bind (:map cider-mode-map
                ("<C-return>" . cider-eval-last-sexp)
                ("<M-return>" . cider-eval-defun-at-point)
                ("<S-return>" . cider-eval-region)
                ("<C-M-return>" . cider-eval-buffer)
                ("<C-S-return>" . cider-eval-print-last-sexp))
    :config
    (unbind-key "C-M-i" cider-mode-map)
    (setq cider-repl-display-help-banner nil
          cider-repl-history-file "~/.emacs.d/cider-history")))

(use-package geiser
  :mode ("\\.scm\\'" . scheme-mode)
  :config
  (setq geiser-default-implementation 'guile)
  (bind-keys :map scheme-mode-map
             ("<C-return>" . geiser-eval-last-sexp)
             ("<M-return>" . geiser-eval-definition)
             ("<S-return>" . geiser-eval-region)
             ("<C-M-return>" . geiser-eval-buffer))
  (use-package quack))

(with-eval-after-load 'prolog
  (use-package ediprolog
    ;; defer with :bind somehow does not work properly for ediprolog
    :config (bind-key "<C-return>" 'ediprolog-dwim prolog-mode-map)))
(setq auto-mode-alist
      (append '(("\\.pl$" . prolog-mode) ("\\.m$" . mercury-mode))
              auto-mode-alist))

(use-package python
  :bind (:map python-mode-map
              ("<M-return>" . python-shell-send-defun)
              ("<S-return>" . python-shell-send-region)
              ("<C-M-return>" . python-shell-send-buffer))
  :config (setq python-shell-interpreter "python3"))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

(use-package ess-site
  :ensure ess
  :commands R
  :bind (:map ess-mode-map
              ("<C-return>" . ess-eval-line)
              ("<M-return>" . ess-eval-function-or-paragraph)
              ("<S-return>" . ess-eval-region)
              ("<C-M-return>" . ess-eval-buffer)))

(use-package org-agenda
  :ensure org
  :bind ("C-c m a" . org-agenda)
  :init
  (setq org-directory "~/Sotha_Sil/Emacs/org"
        org-agenda-files "~/Sotha_Sil/Emacs/org/agenda-files"
        org-archive-location "~/Sotha_Sil/Emacs/org/archive.org::"
        org-log-done 'time
        org-latex-create-formula-image-program 'imagemagick
        org-src-fontify-natively t
        org-latex-packages-alist '(("" "minted"))
        org-latex-listings 'minted
        org-confirm-babel-evaluate nil)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

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
  (use-package markdown-mode+)

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
                      (if (zerop (length EXTENSION)) "pdf" EXTENSION)))))))

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
              (push '("latexmk" "latexmk -pdf -latexoption=-shell-escape %s"
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
    :config
    (ido-vertical-mode 1)
    (setq ido-vertical-show-count t)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))
  (use-package flx-ido
    :config
    (flx-ido-mode 1)
    (setq ido-use-faces nil
          gc-cons-threshold 20000000)))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package avy
  :bind (("C-c j" . avy-goto-word-1) ;; same as ace-jump
         ("C-c l" . avy-goto-char)   ;; letter
         ("C-c k" . avy-goto-char-2) ;; keys
         ("M-g g" . avy-goto-line))
  :config (avy-setup-default))

(use-package projectile
  :bind ("C-c m p" . projectile-global-mode))

;;;;;;;;;;;;;
;; editing ;;
;;;;;;;;;;;;;

(use-package expand-region
  :bind ("S-SPC" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-c m c" . mc/mark-more-like-this-extended)
         ("C-\"" . mc/mark-all-dwim)
         ("C-'" . mc-hide-unmatched-lines-mode)))

(use-package centered-cursor-mode
  :bind ("C-c m l" . global-centered-cursor-mode))

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
  (bind-keys :map smartparens-mode-map
             ;; paredit bindings
             ("C-)" . sp-forward-slurp-sexp)
             ("C-}" . sp-dedent-adjust-sexp)
             ("C-(" . sp-backward-slurp-sexp)
             ("C-{" . sp-backward-barf-sexp)
             ("M-s" . sp-splice-sexp)
             ("M-S" . sp-split-sexp)
             ("M-J" . sp-join-sexp)
             ("M-?" . sp-convolute-sexp)
             ;; more magic added by smartparens
             ("M-(" . sp-indent-adjust-sexp) ; mimic C-(
             ("M-)" . sp-add-to-next-sexp)   ; mimic C-)
             ("M-/" . sp-rewrap-sexp)
             ("C-<" . sp-extract-before-sexp)
             ("C->" . sp-extract-after-sexp)
             ;; navigation via parentheses
             ("M-n" . sp-down-sexp)
             ("M-p" . sp-backward-down-sexp)
             ("M-[" . sp-backward-up-sexp) ; mimic M-< and M-{
             ("M-]" . sp-up-sexp)          ; also replace M-)
             ("<C-left>" . sp-select-previous-thing-exchange)
             ("<C-right>" . sp-select-next-thing)
             ;; emacs stuff ; overrides
             ("C-M-f" . sp-forward-sexp)      ; forward-sexp
             ("C-M-b" . sp-backward-sexp)     ; backward-sexp
             ("C-M-a" . sp-beginning-of-sexp) ; beginning-of-defun
             ("C-M-e" . sp-end-of-sexp)       ; end-of-defun
             ("C-M-n" . sp-next-sexp)         ; forward-list
             ("C-M-p" . sp-previous-sexp)     ; backward-list
             ("C-j" . sp-newline)  ; electric-newline-and-maybe-indent
             ("C-M-t"   . sp-transpose-sexp)        ; transpose-sexp
             ("C-x C-t" . sp-transpose-hybrid-sexp) ; transpose-lines
             ("C-M-k"           . sp-kill-sexp)     ; kill-sexp
             ("<C-M-backspace>" . sp-backward-kill-sexp) ; backward-kill-sexp
             ("<C-backspace>" . sp-splice-sexp-killing-backward)
             ("C-M-d" . sp-splice-sexp-killing-forward) ; down-list
             ("C-M-u" . sp-unwrap-sexp)          ; backward-up-list
             ("C-M-i" . sp-indent-defun)         ; completion-at-point
             ;; strict mode stuff
             ("C-d"   . sp-delete-char)          ; delete-char
             ("DEL"   . sp-backward-delete-char) ; backward-delete-char
             ("M-DEL" . sp-backward-kill-word)   ; backward-kill-word
             ("M-d"   . sp-kill-word)            ; kill-word
             ("C-k"   . sp-kill-hybrid-sexp)     ; kill-line
             )
  (use-package region-bindings-mode
    :config
    (region-bindings-mode-enable)
    (bind-keys :map region-bindings-mode-map
               ("d" . delete-region)
               ("g" . keyboard-quit)
               ("i" . indent-region)
               ("k" . kill-region)
               ("l" . downcase-region)
               ("m" . mc/edit-lines)
               ("r" . replace-string)
               ("u" . upcase-region)
               ("w" . kill-ring-save)
               ("x" . mc/mark-all-in-region)
               (";" . comment-box)))
  
  (use-package undo-tree
    :diminish " ψ"
    :config (global-undo-tree-mode 1))
  
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
    :bind (("<C-tab>" . hippie-expand)
           ("<M-tab>" . crazy-hippie-expand)))

  (global-company-mode 1)
  (projectile-global-mode 1))

(use-package yasnippet
  :bind (("C-c m y" . yas-global-mode)
         :map yas-minor-mode-map
         ("<S-tab>" . yas-expand))
  :diminish (yas-minor-mode . " Y")
  :init (setq yas-snippet-dirs '(yas-installed-snippets-dir))
  :config
  (unbind-key "<tab>" yas-minor-mode-map)
  (unbind-key "TAB" yas-minor-mode-map))

(use-package company
  :bind ("C-c m k" . global-company-mode)
  :diminish " K"
  :config
  (unbind-key "<tab>" company-active-map)
  (unbind-key "TAB" company-active-map)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t)
  (use-package company-flx
    :config (company-flx-mode 1))
  (use-package company-math
    :config (add-to-list 'company-backends 'company-math-symbols-unicode))
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay 1)))

(use-package flycheck
  :bind ("C-c m f" . global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :config
    (setq flycheck-display-errors-function
          #'flycheck-pos-tip-error-messages)))

(use-package flyspell
  :diminish " $"
  :bind ("C-c m $" . flyspell-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/rdallasgray/graphene/blob/master/graphene-helper-functions.el

(defun comment-current-line-dwim ()
  "Comment or uncomment the current line."
  (interactive)
  (save-excursion
    (push-mark (beginning-of-line) t t)
    (end-of-line)
    (comment-dwim nil)))

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

(bind-keys ("C-;" . comment-current-line-dwim)
           ("<S-up>" . increase-window-height)
           ("<S-down>" . decrease-window-height)
           ("<S-right>" . increase-window-width)
           ("<S-left>" . decrease-window-width))

(add-hook 'after-init-hook
          (lambda ()
            (setq initial-scratch-message
                  (concat initial-scratch-message
                          (format ";; Emacs initialized in %.2f seconds.\n\n"
                                  (float-time (time-subtract after-init-time before-init-time)))))))
