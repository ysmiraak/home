;;; .emacs --- ysmiraak's emacs init file.

;; Copyright (C) 2015-2017 ysmiraak

;; Author: ysmiraak <ysmiraak@gmail.com>
;; URL: https://github.com/ysmiraak/home-backup/blob/master/.emacs

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `custom-set-variables' and `custom-set-face' work better than
;; `setq', `setq-default', or `set-face-attributes', but if you let
;; Custum edit them, it will mess up the comments, so be careful.

;;; Code:

(custom-set-variables
 '(custom-file "~/.emacs.d/custom.el")
 '(package-enable-at-startup nil)
 '(same-window-buffer-names '("*Buffer List*"))
 '(uniquify-buffer-name-style 'forward)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(truncate-lines t)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(column-number-mode t)
 '(ring-bell-function 'ignore t)
 ;; cursor
 '(blink-cursor-mode nil)
 '(cursor-type '(bar . 3))
 '(cursor-in-non-selected-windows 'hollow)
 '(fill-column 81)
 ;; always uses spaces for tabs (for real tabs, use C-q)
 ;; see also tabify, untabify, and tab-width
 '(indent-tabs-mode nil)
 '(electric-indent-mode nil)
 ;; clipboard
 '(x-select-enable-clipboard t)
 '(save-interprogram-paste-before-kill t)
 ;; backup
 '(create-lockfiles nil)
 '(auto-save-default nil)
 '(version-control t)
 '(delete-old-versions t)
 '(backup-directory-alist `(("." . ,temporary-file-directory))))

(mapc (lambda (cmd) (put cmd 'disabled nil))
      ;; enable some disabled commands
      '(upcase-region
        downcase-region
        narrow-to-region
        dired-find-alternate-file))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun hook-all (f &rest hs) "Add F for all HS." (mapc (lambda (h) (add-hook h f)) hs))
(defun add-hooks (h &rest fs) "Add to H all FS." (mapc (lambda (f) (add-hook h f)) fs))

(define-key input-decode-map
  ;; free C-m from RET to be used as my personal C-c, for opening
  (if window-system (kbd "C-m") (kbd "C-M-z"))
  ;; black books.
  (kbd "H-m"))

;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t
      use-package-always-ensure t)

(use-package benchmark-init :disabled
  :config (benchmark-init/activate))

(use-package exec-path-from-shell :if (equal 'ns window-system) :demand
  :bind (("<C-s-f>" . toggle-frame-fullscreen)
         ("<C-s-268632070>" . toggle-frame-fullscreen))
  :config
  (toggle-frame-fullscreen)
  (setq insert-directory-program "gls")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "LANG" "LC_ALL" "EMAIL" "RUST_SRC_PATH")))

(use-package rainbow-delimiters :if (display-graphic-p) :demand
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable)
  :config
  (custom-set-faces
   ;; I bow not yet before the Iron Crown,
   '(region ((t (:background "#242424")))) ;; Nirn gray
   ;; nor cast my own small golden sceptre down.
   '(cursor ((t (:background "#DAA520")))) ;; goldenrod
   ;; Ink and gold.
   '(mc/cursor-bar-face ((t (:background "#DAA520" :foreground "#242424")))))
  (use-package zenburn-theme)
  (load-theme 'zenburn t)
  (use-package hl-line)
  (global-hl-line-mode 1)
  (use-package powerline)
  (powerline-center-theme))

(use-package rainbow-mode :defer
  :commands rainbow-mode)

(use-package magit :defer
  :bind ("H-m g" . magit-status))

;;;;;;;;;
;; nav ;;
;;;;;;;;;

(use-package counsel :demand :diminish (ivy-mode . "")
  :bind (("C-s" . swiper) ("M-x" . counsel-M-x))
  :config (ivy-mode 1)
  (use-package flx)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-ignore-order)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))
        ivy-height 13))

(use-package which-key :demand :diminish ""
  :config (which-key-mode 1))

(use-package avy :defer
  :bind (("M-'"  . avy-goto-char-2) ("C-'"  . avy-goto-char-2)
         ("M-\"" . avy-pop-mark)    ("C-\"" . avy-pop-mark)
         ("M-g g" . avy-goto-line)))

(use-package centered-cursor-mode :defer :diminish (centered-cursor-mode . "")
  :bind ("H-m l" . global-centered-cursor-mode))

(use-package ace-window :defer
  :bind ("C-x o" . ace-window))

(use-package windmove :defer
  :bind (("<C-M-left>"  . windmove-left)
         ("<C-M-right>" . windmove-right)
         ("<C-M-up>"    . windmove-up)
         ("<C-M-down>"  . windmove-down)))

(use-package projectile :demand
  :config (projectile-mode 1))

;;;;;;;;;;
;; edit ;;
;;;;;;;;;;

(use-package browse-kill-ring :defer
  :bind ("H-m y" . browse-kill-ring))

(use-package undo-tree :demand :diminish ""
  :config (global-undo-tree-mode 1))

(use-package smartparens-config :demand :diminish (smartparens-mode . "")
  :ensure smartparens
  :config (smartparens-global-mode 1)
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
             ("M-I" . sp-indent-defun)
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
             ("C-S-r" . sp-select-previous-thing-exchange)
             ("C-S-s" . sp-select-next-thing)
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
             ;; strict mode stuff
             ("C-d"   . sp-delete-char)          ; delete-char
             ("DEL"   . sp-backward-delete-char) ; backward-delete-char
             ("M-DEL" . sp-backward-kill-word)   ; backward-kill-word
             ("M-d"   . sp-kill-word)            ; kill-word
             ("C-k"   . sp-kill-hybrid-sexp)     ; kill-line
             )
  (show-smartparens-global-mode t)
  (set-face-attribute 'sp-show-pair-match-face nil ;; ELEGENT WEAPONS
                      :background "#181818"        ;; Star Wound
                      :foreground "#A41210" ;; Tamriel-Aetherius-Oblivion
                      :weight 'bold)
  (set-face-attribute 'sp-show-pair-mismatch-face nil ;; FOR A MORE... CIVILIZED AGE.
                      :background "#161616" ;; the void unknown
                      :foreground "#003B6F" ;; Tardis blue, Mnemoli
                      :weight 'black))

(use-package multiple-cursors :defer
  :bind (("H-m m" . mc/mark-more-like-this-extended)
         ("H-m h" . mc-hide-unmatched-lines-mode)))

(use-package region-bindings-mode :demand
  :bind (:map region-bindings-mode-map
              ("b" . comment-box)
              ("d" . delete-region)
              ("g" . keyboard-quit)
              ("i" . indent-region)
              ("k" . kill-region)
              ("l" . downcase-region)
              ("m" . mc/mark-all-in-region)
              ("n" . mc/edit-lines)
              ("r" . replace-string)
              ("u" . upcase-region)
              ("w" . kill-ring-save)
              (";" . comment-or-uncomment-region))
  :config (region-bindings-mode-enable))

(use-package expand-region :defer
  :bind (("M-`" . er/expand-region) ("C-`" . er/expand-region)))

(use-package drag-stuff :defer
  :bind (("<M-left>"  . drag-stuff-left)
         ("<M-right>" . drag-stuff-right)
         ("<M-up>"    . drag-stuff-up)
         ("<M-down>"  . drag-stuff-down)))

(use-package aggressive-indent :demand :diminish " i"
  :bind ("H-m i" . global-aggressive-indent-mode)
  :config (global-aggressive-indent-mode 1))

(use-package hungry-delete :demand :diminish ""
  :config (global-hungry-delete-mode 1))

(use-package visual-regexp :defer
  :bind ("C-M-%" . vr/query-replace))

(use-package fix-word :defer
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)))

(use-package zzz-to-char :defer
  :bind ("M-z" . zzz-to-char))

(use-package whitespace-cleanup-mode :demand :diminish (whitespace-cleanup-mode . "")
  :config (global-whitespace-cleanup-mode 1))

;;;;;;;;;;
;; lang ;;
;;;;;;;;;;

(use-package company :demand :diminish " K"
  :bind (("M-~" . company-complete) ("C-~" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("M-h" . company-quickhelp-manual-begin))
  :config (global-company-mode 1)
  (unbind-key "<tab>" company-active-map)
  (unbind-key "TAB" company-active-map)
  (unbind-key "M-n" company-active-map)
  (unbind-key "M-p" company-active-map)
  (use-package company-math)
  (push 'company-math-symbols-unicode company-backends)
  (use-package company-quickhelp)
  (company-quickhelp-mode 1)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-quickhelp-delay nil))

(use-package yasnippet :demand :diminish (yas-minor-mode . " Y")
  :config (yas-global-mode 1))

(use-package flyspell :defer :diminish " $"
  :bind ("H-m $" . flyspell-buffer)
  :init (hook-all #'flyspell-mode
                  'org-mode-hook
                  'LaTeX-mode-hook
                  'markdown-mode-hook))

(use-package flycheck :defer
  :init (hook-all #'flycheck-mode
                  'emacs-lisp-mode-hook
                  'geiser-mode-hook
                  'shell-mode-hook
                  'rust-mode-hook
                  'LaTeX-mode-hook
                  'markdown-mode-hook
                  'css-mode-hook
                  'html-mode-hook
                  'js2-mode-hook)
  :config (use-package flycheck-pos-tip)
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package eldoc :demand :diminish ""
  :bind (:map lisp-mode-shared-map
              ("M-," . eval-last-sexp)       ("C-," . eval-last-sexp)
              ("M-." . eval-defun)           ("C-." . eval-defun)
              ("M-=" . eval-region)          ("C-=" . eval-region)
              ("M-+" . eval-print-last-sexp) ("C-+" . eval-print-last-sexp)
              ("C-M-=" . eval-buffer))
  :init (hook-all #'eldoc-mode
                  'emacs-lisp-mode-hook
                  'lisp-interaction-mode-hook
                  'ielm-mode-hook
                  'clojure-mode-hook
                  'cider-repl-mode-hook
                  'idris-mode-hook
                  'rust-mode-hook))

(use-package eval-sexp-fu :demand
  ;; delete the *.elc files in this package !!!!
  :init (setq byte-compile-warnings nil)
  :config (setq byte-compile-warnings t)
  (setq eval-sexp-fu-flash-face 'region
        eval-sexp-fu-flash-duration eval-sexp-fu-flash-error-duration))

(use-package clojure-mode :defer
  :init (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  :bind (("H-m s" . cider-scratch)
         :map cider-mode-map
         ("M-," . cider-eval-last-sexp)       ("C-," . cider-eval-last-sexp)
         ("M-." . cider-eval-defun-at-point)  ("C-." . cider-eval-defun-at-point)
         ("M-=" . cider-eval-region)          ("C-=" . cider-eval-region)
         ("M-+" . cider-eval-print-last-sexp) ("C-+" . cider-eval-print-last-sexp)
         ("C-M-=" . cider-eval-buffer))
  :config (use-package cider)
  (use-package cider-eval-sexp-fu)
  (use-package clj-refactor)
  (cljr-add-keybindings-with-prefix "H-m r")
  (setq cljr-suppress-middleware-warnings t
        nrepl-hide-special-buffers t
        cider-font-lock-dynamically t
        cider-prefer-local-resources t
        cider-allow-jack-in-without-project t
        cider-doc-xref-regexp "\\[\\[\\(.*?\\)\\]\\]"
        cider-repl-history-file "~/.emacs.d/cider-history")
  (use-package kibit-helper))

(use-package geiser :defer
  :defines geiser-active-implementations
  :bind (:map scheme-mode-map
              ("M-," . geiser-eval-last-sexp)  ("C-," . geiser-eval-last-sexp)
              ("M-." . geiser-eval-definition) ("C-." . geiser-eval-definition)
              ("M-=" . geiser-eval-region)     ("C-=" . geiser-eval-region)
              ("C-M-=" . geiser-eval-buffer))
  :config
  (setq geiser-active-implementations '(chez))
  (use-package quack))

(use-package ediprolog :defer
  :mode ("\\.pl$" . prolog-mode)
  :defines prolog-mode-map
  :init (with-eval-after-load 'prolog
          (bind-keys :map prolog-mode-map
                     ("M-=" . ediprolog-dwim)
                     ("C-=" . ediprolog-dwim))))

(use-package haskell-mode :defer)

(use-package idris-mode :defer
  :init (add-hook 'idris-mode-hook (lambda () (aggressive-indent-mode 0)))
  :bind (:map idris-mode-map
              ("M-," . idris-case-dwim)    ("C-," . idris-case-dwim)
              ("M-." . idris-add-clause)   ("C-." . idris-add-clause)
              ("M-=" . idris-proof-search) ("C-=" . idris-proof-search)
              ("C-M-=" . idris-load-file)
              ("C-c C-q" . idris-quit)))

(use-package ess :defer
  :commands R
  :bind (:map ess-mode-map
              ("M-," . ess-eval-line)                  ("C-," . ess-eval-line)
              ("M-." . ess-eval-function-or-paragraph) ("C-." . ess-eval-function-or-paragraph)
              ("M-=" . ess-eval-region)                ("C-=" . ess-eval-region)
              ("C-M-=" . ess-eval-buffer)))

(use-package elpy :defer
  :init (add-hook 'python-mode-hook #'elpy-mode)
  (add-hook 'elpy-mode-hook
            (lambda ()
              (highlight-indentation-mode 0)
              (aggressive-indent-mode 0)
              (unbind-key "M-=" elpy-mode-map)))
  :bind (:map python-mode-map
              ("M-="   . python-shell-send-region)
              ("C-M-=" . python-shell-send-defun)
              ("M-+"   . python-shell-send-buffer))
  :config (elpy-enable))

(use-package rust-mode :defer
  :init (add-hooks 'rust-mode-hook
                   #'cargo-minor-mode
                   #'flycheck-rust-setup
                   #'racer-mode)
  :config (use-package cargo)
  (use-package flycheck-rust)
  (use-package racer)
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

(use-package js2-mode :defer
  :mode ("\\.js\\'" . js2-mode))

(use-package csv-mode :defer)

(use-package markdown-mode :defer
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.[Rr]md\\'" . markdown-mode))
  :config (setq markdown-enable-math t)
  (use-package markdown-mode+)
  (defun rmarkdown-render-current-file-then-display (&optional EXTENSION)
    "Output format should be specified accordingly in YAML."
    (interactive "sOutput filename extension (default pdf): ")
    (shell-command
     (format "Rscript -e 'rmarkdown::render(\"%s\")'; and open %s"
             (shell-quote-argument (buffer-file-name))
             (shell-quote-argument
              (concat (file-name-sans-extension (buffer-file-name)) "."
                      (if (zerop (length EXTENSION)) "pdf" EXTENSION)))))))

(use-package tex :defer
  :ensure auctex
  :init (add-hooks 'LaTeX-mode-hook
                   #'LaTeX-math-mode
                   #'latex-preview-pane-enable
                   #'turn-on-cdlatex
                   #'turn-on-reftex)
  :config (setq-default TeX-engine 'xetex)
  (push '("xelatexmk"
          "latexmk -pdf -pdflatex=\"xelatex -interaction=nonstopmode -shell-escape -synctex=1\" %s"
          TeX-run-TeX nil t :help "run xelatexmk on file")
        TeX-command-list)
  (add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "xelatexmk")))
  ;; Skim -> Preferences -> Sync; CMD + shift + click in the pdf file for jumping to source
  (push '("skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
        TeX-view-program-list)
  (push '(output-pdf "skim") TeX-view-program-selection)
  (server-start)
  (setq TeX-auto-save t
        TeX-parse-self t)
  (use-package latex-preview-pane)
  (use-package cdlatex)
  (unbind-key "(" cdlatex-mode-map)
  (unbind-key "<" cdlatex-mode-map)
  (unbind-key "[" cdlatex-mode-map)
  (unbind-key "{" cdlatex-mode-map)
  (use-package reftex)
  (setq reftex-plug-into-AUCTeX t)
  (use-package company-auctex)
  (with-eval-after-load 'company
    (company-auctex-init)))

(use-package org :defer
  :bind ("H-m a" . org-agenda)
  :init (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (setq org-directory "~/sotha_sil/emacs/org"
        org-agenda-files "~/sotha_sil/emacs/org/agenda-files"
        org-archive-location "~/sotha_sil/emacs/org/archive.org::"
        org-log-done 'time)
  :defines org-latex-listings
  :config (unbind-key "C-'" org-mode-map)
  (setq org-latex-create-formula-image-program 'imagemagick
        org-latex-listings 'minted
        org-src-fontify-natively t
        org-latex-default-packages-alist
        '(("" "fontspec" t)
          ("" "graphicx" t)
          ("" "longtable" nil)
          ("" "float" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "hyperref" nil))))

(provide '.emacs)
;;; .emacs ends here
