(set-language-environment "utf-8")

(if (eq system-type 'darwin)
    (defvar cask-path "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (progn (defvar cask-path "/usr/share/emacs/site-lisp/cask/cask.el")
	 (unless (file-exists-p cask-path)
	   (setq cask-path "~/.nix-profile/cask.el"))))
(require 'cask cask-path)
(cask-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-master nil)
 '(TeX-parse-self t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("92f826e9660492a15f363891cd9c128eb381ebe2ba1efe804224c895e742bfa2" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "cdbd2c07cda87348734c588b9d6d300af4e073f7b8158ab1dfba00be84b37ca6" "a1ea1b279f80fdb7868418563bd417faf144ad21dda217a17c52548606adcdd7" default)))
 '(evil-want-C-u-scroll t)
 '(fci-rule-color "#424242")
 '(global-linum-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(linum-format "%d ")
 '(magit-diff-refine-hunk t)
 '(magit-popup-use-prefix-argument nil)
 '(org-checkbox-hierarchical-statistics t)
 '(org-default-notes-file "~/Documents/Org/todo.org")
 '(org-ellipsis "⤵")
 '(org-export-with-statistics-cookies t)
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values (quote ((TeX-engine . xelatex))))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil)
 '(vc-handled-backends nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 90 :family "Dina")))))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(defvar pdfviewer "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
(unless (eq system-type 'darwin)
  (setq pdfviewer "mupdf-x11 %o"))
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     `(("PDF Viewer" , pdfviewer)))

(use-package reftex
  :defer t
  :diminish reftex-mode
  :init
  (progn
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)))

;;; All text modes (also AucTeX)
;; Enable auto-fill mode
(use-package auto-fill
  :defer t
  :diminish auto-fill-mode
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill))

(use-package ido
  :config
  (ido-mode 1))

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (progn
    ;; Set path for AucTeX
    (setenv "PATH"
	    (concat "/usr/bin/" ":"
		    "/usr/texbin" ":"
		    "/usr/local/bin" ":"
		    "/usr/local/sbin" ":"
		    (getenv "PATH")))
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    ;; Run latexmk using C-c C-c
    (add-hook 'LaTeX-mode-hook (lambda ()
				 (push
				  '("latexmk" "latexmk %s" TeX-run-TeX nil t
				    :help "Run latexmk on file")
				  TeX-command-list)))
    (add-hook 'LaTeX-mode-hook (lambda ()
				 (push
				  '("pdflatexmk" "latexmk -pdf -e \'$pdflatex=q{pdflatex -synctex=1 %O %s.tex}\'" TeX-run-TeX nil t
				    :help "Run latexmk on file with pdflatex")
				  TeX-command-list)))
    (add-hook 'LaTeX-mode-hook (lambda ()
				 (push
				  '("xelatexmk" "latexmk -xelatex %s" TeX-run-TeX nil t
				    :help "Run latexmk on file with xelatex")
				  TeX-command-list)))
    ;; Set default to latexmk
    (add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))))

(use-package org
  :defer t
  :config
  (progn
    (setq org-todo-keywords
	  '((sequence "TODO" "IN-PROGRESS" "DONE")))
    ;; Set colors of TODO states
    ; (let ((yellow (x-get-resource "color3" ""))
    ; 	  (cyan (x-get-resource "color6" ""))
    ; 	  (green (x-get-resource "color2" "")))
    ;   (setq org-todo-keyword-faces
    ; 	     `(("TODO" :foreground ,yellow :weight bold)
    ; 	       ("IN-PROGRESS" :foreground ,cyan :weight bold)
    ; 	       ("DONE" :foreground ,green :weight bold))))
    (setq org-todo-keyword-faces
	  '(("IN-PROGRESS" org-ellipsis)))
    (setq org-capture-templates
	  '(("t" "Todo" entry (file org-default-notes-file)
	     "* TODO %? %^g")))
    ;; Enable indent mode
    (add-hook 'org-mode-hook 'org-indent-mode)))

(use-package evil
  :init
  (progn
    (require 'evil)
    (evil-mode 1)))

(use-package magit
  :bind (("C-x g" . magit-status) ("C-x G" . magit-dispatch-popup)))

(use-package linum-off
  :init
  (progn
    (require 'linum-off)))

(use-package hlinum
  :init
  (progn
    (require 'hlinum)
    (hlinum-activate)))
