;;; init --- ysmiraak's emacs init file.

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
 '(custom-file (concat user-emacs-directory "custom.el"))
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
 '(select-enable-clipboard t)
 '(save-interprogram-paste-before-kill t)
 ;; backup
 '(create-lockfiles nil)
 '(auto-save-default nil)
 '(version-control t)
 '(delete-old-versions t)
 '(backup-directory-alist `(("." . ,temporary-file-directory)))
 ;; package
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (list 'benchmark-init 'delight 'bind-key 'exec-path-from-shell
         'zenburn-theme 'powerline 'rainbow-delimiters
         'magit 'projectile 'counsel 'flx 'smex 'which-key 'avy 'centered-cursor-mode 'ace-window
         'browse-kill-ring 'undo-tree 'smartparens 'multiple-cursors 'visual-regexp 'expand-region 'drag-stuff 'aggressive-indent 'hungry-delete 'fix-word 'zzz-to-char 'whitespace-cleanup-mode
         'company 'company-quickhelp 'yasnippet 'flycheck 'flycheck-pos-tip
         'clojure-mode 'cider 'clj-refactor 'kibit-helper 'geiser 'ediprolog 'haskell-mode 'idris-mode
         'ess 'elpy 'js2-mode 'rainbow-mode 'csv-mode 'markdown-mode
         'rust-mode 'cargo 'flycheck-rust 'racer
         'latex-preview-pane 'cdlatex 'company-auctex

         'region-bindings-mode )))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun hook-all (f &rest hs) "Add F for all HS." (mapc (lambda (h) (add-hook h f)) hs))
(defun add-hooks (h &rest fs) "Add to H all FS." (mapc (lambda (f) (add-hook h f)) fs))


(mapc (lambda (cmd) (put cmd 'disabled nil))
      '(upcase-region
        downcase-region
        narrow-to-region
        dired-find-alternate-file))

(require 'package)
(push '("melpa" . "http://melpa.org/packages/") package-archives)
;; (push '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") package-archives)
(package-initialize nil)
(require 'benchmark-init)
(benchmark-init/activate)
(require 'delight)
(require 'bind-key)
(bind-keys ("C-M-_" . toggle-truncate-lines)
           ("C-M--" . toggle-truncate-lines)
           ("M-#" . linum-mode))

(defvar flyspell-mode-map)
(defvar cider-mode-map)
(defvar nrepl-hide-special-buffers)
(defvar cider-font-lock-dynamically)
(defvar cider-prefer-local-resources)
(defvar cider-allow-jack-in-without-project)
(defvar cider-doc-xref-regexp)
(defvar cider-repl-history-file)
(defvar cljr-suppress-middleware-warnings)
(defvar scheme-mode-map)
(defvar geiser-active-implementations)
(defvar prolog-mode-map)
(defvar idris-mode-map)
(defvar ess-mode-map)
(defvar python-mode-map)
(defvar racer-rust-src-path)
(defvar markdown-enable-math)
(defvar TeX-command-default)
(defvar TeX-auto-save)
(defvar TeX-parse-self)
(defvar reftex-plug-into-AUCTeX)
(defvar cdlatex-mode-map)
(defvar TeX-command-list)
(defvar TeX-command-list)
(defvar TeX-view-program-list)
(defvar TeX-view-program-list)
(defvar TeX-view-program-selection)
(defvar TeX-view-program-selection)
(defvar org-mode-map)
(defvar org-directory)
(defvar org-agenda-files)
(defvar org-archive-location)
(defvar org-log-done)
(defvar org-latex-create-formula-image-program)
(defvar org-latex-listings)
(defvar org-src-fontify-natively)
(defvar org-latex-default-packages-alist)

(defalias 'cargo-minor-mode 'eq)
(defalias 'flycheck-rust-setup 'eq)
(defalias 'racer-mode 'eq)
(defalias 'LaTeX-math-mode 'eq)
(defalias 'turn-on-org-cdlatex 'eq)
(defalias 'exec-path-from-shell-getenv 'eq)

;;;;;;;;;;
;; face ;;
;;;;;;;;;;

(when (display-graphic-p)
  (custom-set-faces
   '(region ((t (:background "#242424"))))
   '(cursor ((t (:background "#DAA520"))))
   '(mc/cursor-bar-face ((t (:background "#DAA520" :foreground "#242424")))))
  (require 'zenburn-theme)
  (load-theme 'zenburn t)
  (require 'hl-line)
  (global-hl-line-mode 1)
  (require 'powerline)
  (powerline-center-theme)
  (require 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(when (equal 'ns window-system)
  (toggle-frame-fullscreen)
  (bind-keys ("<C-s-f>" . toggle-frame-fullscreen) ("<C-s-268632070>" . toggle-frame-fullscreen))
  (setq insert-directory-program "gls")
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "LANG" "LC_ALL" "EMAIL")))

;;;;;;;;;
;; nav ;;
;;;;;;;;;

(autoload 'magit-status "magit")
(bind-key "M-G" 'magit-status)

(require 'projectile)
(setq projectile-switch-project-action 'projectile-dired
      projectile-completion-system 'ivy)
(delight 'projectile-mode '(:eval (format " [%s]" (projectile-project-name))) 'projectile)
(projectile-mode 1)

(require 'counsel)
(require 'flx)
(require 'smex)
(setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-ignore-order)
        (swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy))
      ivy-height 13)
(delight 'ivy-mode nil 'ivy)
(ivy-mode 1)
(bind-keys ("M-s" . swiper) ("M-x" . counsel-M-x))

(require 'which-key)
(delight 'which-key-mode nil 'which-key)
(which-key-mode 1)

(mapc (lambda (cmd) (autoload cmd "avy"))
      '(avy-goto-char-2
        avy-pop-mark
        avy-goto-line))
(bind-keys ("M-'" . avy-goto-char-2)
           ("M-\"" . avy-pop-mark)
           ("M-g g" . avy-goto-line)
           ("M-g M-g" . avy-goto-line))

(autoload 'centered-cursor-mode "centered-cursor-mode")
(bind-key "M-L" 'global-centered-cursor-mode)

(autoload 'ace-window "ace-window")
(bind-key "C-x o" 'ace-window)

;;;;;;;;;;
;; edit ;;
;;;;;;;;;;

(autoload 'browse-kill-ring "'browse-kill-ring")
(bind-key "C-M-y" 'browse-kill-ring)

(require 'undo-tree)
(delight 'undo-tree-mode nil 'undo-tree)
(bind-key "C--" 'undo-tree-undo)
(global-undo-tree-mode 1)

(autoload 'hippie-expand "hippie-exp")
(bind-key "M-/" 'hippie-expand)
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
        try-complete-lisp-symbol))

(require 'smartparens)
(require 'smartparens-config)
(delight 'smartparens-mode nil 'smartparens)
(set-face-attribute 'sp-show-pair-match-face    nil :background "#181818" :foreground "#A41210" :weight 'bold)
(set-face-attribute 'sp-show-pair-mismatch-face nil :background "#161616" :foreground "#003B6F" :weight 'black)
(bind-keys :map smartparens-mode-map
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
           ("C-M-k" . sp-kill-sexp)
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
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

(mapc (lambda (cmd) (autoload cmd "mc-mark-more"))
      '(mc/mark-more-like-this-extended
        mc/mark-all-in-region
        mc/mark-lines))
(bind-key "M-M" 'mc/mark-more-like-this-extended)

(mapc (lambda (cmd) (autoload cmd "visual-regexp"))
      '(vr/query-replace
        vr/replace
        vr/mc-mark))
(bind-key "C-M-%" 'vr/query-replace)

(require 'region-bindings-mode)
(bind-keys :map region-bindings-mode-map
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
(region-bindings-mode-enable)

(autoload 'er/expand-region "expand-region")
(bind-key "M-h" 'er/expand-region)

(mapc (lambda (cmd) (autoload cmd "drag-stuff"))
      '(drag-stuff-left
        drag-stuff-right
        drag-stuff-down
        drag-stuff-up))
(bind-keys ("<M-left>" . drag-stuff-left)
           ("<M-right>" . drag-stuff-right)
           ("<M-down>" . drag-stuff-down)
           ("<M-up>" . drag-stuff-up))

(require 'aggressive-indent)
(delight 'aggressive-indent-mode " i" 'aggressive-indent)
(bind-key "M-I" 'global-aggressive-indent-mode)
(global-aggressive-indent-mode 1)

(require 'hungry-delete)
(delight 'hungry-delete-mode " d" 'hungry-delete)
(bind-key "M-D" 'global-hungry-delete-mode)
(global-hungry-delete-mode 1)

(mapc (lambda (cmd) (autoload cmd "fix-word"))
      '(fix-word-upcase
        fix-word-downcase
        fix-word-capitalize))
(bind-keys ("M-u" . fix-word-upcase)
           ("M-l" . fix-word-downcase)
           ("M-c" . fix-word-capitalize))

(autoload 'zzz-to-char "zzz-to-char")
(bind-key "M-z" 'zzz-to-char)

(require 'whitespace-cleanup-mode)
(delight 'whitespace-cleanup-mode nil 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode 1)

;;;;;;;;;;
;; lang ;;
;;;;;;;;;;

(require 'company)
(require 'company-quickhelp)
(delight 'company-mode nil 'company)
(bind-keys ("C-M-i" . company-complete)
           :map company-active-map
           ("M-h" . company-quickhelp-manual-begin))
(unbind-key "<tab>" company-active-map)
(unbind-key "TAB" company-active-map)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 2
      company-tooltip-align-annotations t
      company-selection-wrap-around t
      company-quickhelp-delay nil)
(global-company-mode 1)
(company-quickhelp-mode 1)

(require 'yasnippet)
(delight 'yas-minor-mode nil 'yasnippet)
(yas-global-mode 1)

(hook-all #'flyspell-mode
          'org-mode-hook
          'LaTeX-mode-hook
          'markdown-mode-hook)
(with-eval-after-load 'flyspell
  (delight 'flyspell-mode " $" 'flyspell)
  (bind-key "C-;" 'flyspell-correct-word-before-point flyspell-mode-map))

(hook-all #'flycheck-mode
          'emacs-lisp-mode-hook
          'geiser-mode-hook
          'shell-mode-hook
          'rust-mode-hook
          'LaTeX-mode-hook
          'markdown-mode-hook
          'css-mode-hook
          'html-mode-hook
          'js2-mode-hook)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(require 'eldoc)
(delight 'eldoc-mode nil 'eldoc)
(bind-keys ("C-j" . newline-and-indent)
           :map lisp-mode-shared-map
           ("M-RET" . eval-last-sexp)
           ("C-M-x" . eval-defun)
           ("C-M-z" . eval-region))
(hook-all #'eldoc-mode
          'emacs-lisp-mode-hook
          'lisp-interaction-mode-hook
          'ielm-mode-hook
          'clojure-mode-hook
          'cider-repl-mode-hook
          'idris-mode-hook
          'rust-mode-hook)

(add-hook 'clojure-mode-hook #'clj-refactor-mode)
(autoload 'cider-scratch "cider-scratch")
(bind-key "M-S" 'cider-scratch)
(with-eval-after-load 'clojure-mode
  (require 'cider)
  (require 'clj-refactor)
  (require 'kibit-helper)
  (bind-keys :map cider-mode-map
             ("M-RET" . cider-eval-last-sexp)
             ("C-M-x" . cider-eval-defun-at-point)
             ("C-M-z" . cider-eval-region))
  (cljr-add-keybindings-with-prefix "M-R")
  (setq cljr-suppress-middleware-warnings t
        nrepl-hide-special-buffers t
        cider-font-lock-dynamically t
        cider-prefer-local-resources t
        cider-allow-jack-in-without-project t
        cider-doc-xref-regexp "\\[\\[\\(.*?\\)\\]\\]"
        cider-repl-history-file "~/.emacs.d/cider-history"))

(with-eval-after-load 'geiser
  (bind-keys :map scheme-mode-map
             ("M-RET" . geiser-eval-last-sexp)
             ("C-M-x" . geiser-eval-definition)
             ("C-M-z" . geiser-eval-region))
  (setq geiser-active-implementations '(chez)))

(autoload 'prolog-mode "prolog")
(push '("\\.pl$" . prolog-mode) auto-mode-alist)
(with-eval-after-load 'prolog
  (bind-key "M-RET" 'ediprolog-dwim prolog-mode-map))

(add-hook 'idris-mode-hook (lambda () (aggressive-indent-mode -1)))
(with-eval-after-load 'idris-mode
  (bind-keys :map idris-mode-map
             ("M-RET" . idris-case-dwim)
             ("C-M-x" . idris-add-clause)
             ("C-M-z" . idris-proof-search)
             ("C-c C-q" . idris-quit)))

(with-eval-after-load 'ess
  (bind-keys :map ess-mode-map
             ("M-RET" . ess-eval-line)
             ("C-M-x" . ess-eval-function-or-paragraph)
             ("C-M-z" . ess-eval-region)))

(add-hook 'python-mode-hook
          (lambda ()
            (elpy-mode 1)
            (highlight-indentation-mode -1)
            (aggressive-indent-mode -1)))
(with-eval-after-load 'python-mode
  (bind-keys :map python-mode-map
             ("C-M-x" . python-shell-send-defun)
             ("C-M-z" . python-shell-send-region))
  (require 'elpy)
  (elpy-enable))

(add-hooks 'rust-mode-hook
           #'cargo-minor-mode
           #'flycheck-rust-setup
           #'racer-mode)
(with-eval-after-load 'rust-mode
  (require 'cargo)
  (require 'flycheck-rust)
  (require 'racer)
  (setq racer-rust-src-path
        (funcall (if (equal 'ns window-system)
                     #'exec-path-from-shell-getenv
                   #'getenv)
                 "RUST_SRC_PATH")))

(autoload 'js2-mode "js2-mode")
(push '("\\.js\\'" . js2-mode) auto-mode-alist)

(autoload 'rainbow-mode "rainbow-mode")

(autoload 'gfm-mode "markdown-mode")
(autoload 'markdown-mode "markdown-mode")
(push '("README\\.md\\'" . gfm-mode) auto-mode-alist)
(push '("\\.[Rr]md\\'" . markdown-mode) auto-mode-alist)
(with-eval-after-load 'markdown-mode
  (setq markdown-enable-math t))

(add-hooks 'LaTeX-mode-hook
           #'LaTeX-math-mode
           #'latex-preview-pane-enable
           #'turn-on-cdlatex
           #'turn-on-reftex)
(add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "xelatexmk")))
(with-eval-after-load 'tex
  (setq-default TeX-engine 'xetex)
  (setq TeX-auto-save t
        TeX-parse-self t)
  (require 'reftex)
  (setq reftex-plug-into-AUCTeX t)
  (require 'latex-preview-pane)
  (require 'cdlatex)
  (unbind-key "(" cdlatex-mode-map)
  (unbind-key "<" cdlatex-mode-map)
  (unbind-key "[" cdlatex-mode-map)
  (unbind-key "{" cdlatex-mode-map)
  (require 'company-auctex)
  (with-eval-after-load 'company
    (company-auctex-init))
  (push '("xelatexmk"
          "latexmk -pdf -pdflatex=\"xelatex -interaction=nonstopmode -shell-escape -synctex=1\" %s"
          TeX-run-TeX nil t :help "run xelatexmk on file")
        TeX-command-list)
  ;; Skim -> Preferences -> Sync; CMD + shift + click in the pdf file for jumping to source
  (push '("skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
        TeX-view-program-list)
  (push '(output-pdf "skim") TeX-view-program-selection)
  (server-start))

(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
(autoload 'org-agenda "org-agenda")
(bind-key "M-A" 'org-agenda)
(with-eval-after-load 'org
  (unbind-key "C-'" org-mode-map)
  (unbind-key "M-h" org-mode-map)
  (setq org-directory "~/sotha_sil/emacs/org"
        org-agenda-files "~/sotha_sil/emacs/org/agenda-files"
        org-archive-location "~/sotha_sil/emacs/org/archive.org::"
        org-log-done 'time
        org-latex-create-formula-image-program 'imagemagick
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

(provide 'init)
;;; init.el ends here
