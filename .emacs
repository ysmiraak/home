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
  :bind ("M-G" . magit-status))

;;;;;;;;;
;; nav ;;
;;;;;;;;;

(bind-key "C-M-_" 'toggle-truncate-lines)
(bind-key "C-M--" 'toggle-truncate-lines)

(use-package linum :defer
  :bind ("M-#" . linum-mode))

(use-package counsel :demand :diminish (ivy-mode . "")
  :bind (("M-s" . swiper) ("M-x" . counsel-M-x))
  :config (ivy-mode 1)
  (use-package flx)
  (use-package smex)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-ignore-order)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))
        ivy-height 13))

(use-package which-key :demand :diminish ""
  :config (which-key-mode 1))

(use-package avy :defer
  :bind (("M-'" . avy-goto-char-2)
         ("M-\"" . avy-pop-mark)
         ("M-g g" . avy-goto-line)
         ("M-g M-g" . avy-goto-line)))

(use-package centered-cursor-mode :defer :diminish (centered-cursor-mode . "")
  :bind ("M-L" . global-centered-cursor-mode))

(use-package ace-window :defer
  :bind ("C-x o" . ace-window))

(use-package windmove :defer
  :bind (("M-@ b" . windmove-left)
         ("M-@ f" . windmove-right)
         ("M-@ n" . windmove-down)
         ("M-@ p" . windmove-up)))

(use-package projectile :demand
  :config (projectile-mode 1)
  (setq projectile-switch-project-action 'projectile-dired
        projectile-completion-system 'ivy))

;;;;;;;;;;
;; edit ;;
;;;;;;;;;;

(use-package browse-kill-ring :defer
  :bind ("C-M-y" . browse-kill-ring))

(use-package undo-tree :demand :diminish ""
  :bind ("C--" . undo-tree-undo)
  :config (global-undo-tree-mode 1))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-visible
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-whole-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package smartparens :demand :diminish ""
  :bind (:map smartparens-mode-map
              ;; nav
              ("M-n" . sp-next-sexp)
              ("M-p" . sp-previous-sexp)
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-M-n" . sp-select-next-thing)
              ("C-M-p" . sp-select-previous-thing-exchange)
              ("C-M-d" . sp-down-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ;; edit
              ("C-M-o" . sp-splice-sexp)
              ("C-M-j" . sp-split-sexp)
              ("C-M-h" . sp-join-sexp)
              ("C-M-q" . sp-rewrap-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-k" . sp-kill-sexp)
              ;; transform
              ("M-[" . sp-absorb-sexp)
              ("M-]" . sp-forward-slurp-sexp)
              ("M-{" . sp-extract-before-sexp)
              ("M-}" . sp-forward-barf-sexp)
              ("M-(" . sp-splice-sexp-killing-backward)
              ("M-)" . sp-splice-sexp-killing-forward)
              ("M-*" . sp-raise-sexp)
              ("M-+" . sp-convolute-sexp)
              ("C-M-t"   . sp-transpose-sexp)
              ("C-x C-t" . sp-transpose-hybrid-sexp)
              ;; strict
              ("C-d"   . sp-delete-char)
              ("DEL"   . sp-backward-delete-char)
              ("M-d"   . sp-kill-word)
              ("M-DEL" . sp-backward-kill-word)
              ("C-k"   . sp-kill-hybrid-sexp))
  :config (use-package smartparens-config)
  (smartparens-global-mode 1)
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
  :bind ("M-M" . mc/mark-more-like-this-extended))

(use-package region-bindings-mode :demand
  :bind (:map region-bindings-mode-map
              ("b" . comment-box)
              ("d" . delete-region)
              ("g" . keyboard-quit)
              ("i" . indent-region)
              ("k" . kill-region)
              ("l" . downcase-region)
              ("m" . mc/mark-all-in-region)
              ("M" . vr/mc-mark)
              ("n" . mc/edit-lines)
              ("r" . replace-string)
              ("R" . vr/replace)
              ("u" . upcase-region)
              ("w" . kill-ring-save)
              (";" . comment-or-uncomment-region)
              ("$" . flyspell-region))
  :config (region-bindings-mode-enable))

(use-package expand-region :defer
  :bind (("M-h" . er/expand-region)))

(use-package drag-stuff :defer
  :bind (("<M-left>" . drag-stuff-left)
         ("<M-right>" . drag-stuff-right)
         ("<M-down>" . drag-stuff-down)
         ("<M-up>" . drag-stuff-up)))

(use-package aggressive-indent :demand :diminish " i"
  :bind ("M-I" . global-aggressive-indent-mode)
  :config (global-aggressive-indent-mode 1))

(use-package hungry-delete :demand :diminish " d"
  :bind ("M-D" . global-hungry-delete-mode)
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

(defun hook-all (f &rest hs) "Add F for all HS." (mapc (lambda (h) (add-hook h f)) hs))
(defun add-hooks (h &rest fs) "Add to H all FS." (mapc (lambda (f) (add-hook h f)) fs))

(use-package company :demand :diminish ""
  :bind (("C-M-i" . company-complete)
         :map company-active-map
         ("M-h" . company-quickhelp-manual-begin))
  :config (global-company-mode 1)
  (unbind-key "<tab>" company-active-map)
  (unbind-key "TAB" company-active-map)
  (use-package company-quickhelp)
  (company-quickhelp-mode 1)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-quickhelp-delay nil))

(use-package yasnippet :demand :diminish (yas-minor-mode . "")
  :config (yas-global-mode 1))

(use-package flyspell :defer :diminish " $"
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-word-before-point))
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
  :bind (("C-j" . newline-and-indent)
         :map lisp-mode-shared-map
         ("M-RET" . eval-last-sexp)
         ("C-M-x" . eval-defun)
         ("C-M-z" . eval-region))
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
  :bind (("M-S" . cider-scratch)
         :map cider-mode-map
         ("M-RET" . cider-eval-last-sexp)
         ("C-M-x" . cider-eval-defun-at-point)
         ("C-M-z" . cider-eval-region))
  :config (use-package cider)
  (use-package cider-eval-sexp-fu)
  (use-package clj-refactor)
  (cljr-add-keybindings-with-prefix "M-R")
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
              ("M-RET" . geiser-eval-last-sexp)
              ("C-M-x" . geiser-eval-definition)
              ("C-M-z" . geiser-eval-region))
  :config
  (setq geiser-active-implementations '(chez))
  (use-package quack))

(use-package ediprolog :defer
  :mode ("\\.pl$" . prolog-mode)
  :defines prolog-mode-map
  :init (with-eval-after-load 'prolog
          (bind-key "M-RET" 'ediprolog-dwim prolog-mode-map)))

(use-package haskell-mode :defer)

(use-package idris-mode :defer
  :init (add-hook 'idris-mode-hook (lambda () (aggressive-indent-mode -1)))
  :bind (:map idris-mode-map
              ("M-RET" . idris-case-dwim)
              ("C-M-x" . idris-add-clause)
              ("C-M-z" . idris-proof-search)
              ("C-c C-q" . idris-quit)))

(use-package ess :defer
  :commands R
  :bind (:map ess-mode-map
              ("M-RET" . ess-eval-line)
              ("C-M-x" . ess-eval-function-or-paragraph)
              ("C-M-z" . ess-eval-region)))

(use-package elpy :defer
  :init (add-hook 'python-mode-hook #'elpy-mode)
  (add-hook 'elpy-mode-hook
            (lambda ()
              (highlight-indentation-mode -1)
              (aggressive-indent-mode -1)))
  :bind (:map python-mode-map
              ("C-M-x" . python-shell-send-defun)
              ("C-M-z"   . python-shell-send-region))
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
  :bind ("M-A" . org-agenda)
  :init (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (setq org-directory "~/sotha_sil/emacs/org"
        org-agenda-files "~/sotha_sil/emacs/org/agenda-files"
        org-archive-location "~/sotha_sil/emacs/org/archive.org::"
        org-log-done 'time)
  :defines org-latex-listings
  :config
  (unbind-key "C-'" org-mode-map)
  (unbind-key "M-h" org-mode-map)
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
