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
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (xresources)))
 '(custom-safe-themes
   (quote
    ("7d2447bfa3b440e4eb985c6d199afb25c1b71ea8179066b81a09915ebf3aa95e" default)))
 '(evil-want-C-u-scroll t)
 '(global-linum-mode t)
 '(linum-format "%d ")
 '(magit-diff-refine-hunk t)
 '(org-ellipsis "⤵")
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values (quote ((TeX-engine . xelatex))))
 '(tool-bar-mode nil))
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
  ;:commands (org-mode org-indent-mode)
  :config
  (progn
    (setq org-todo-keywords
	  '((sequence "TODO" "IN-PROGRESS" "DONE")))
    (let ((yellow (x-get-resource "color3" ""))
	  (cyan (x-get-resource "color6" ""))
	  (green (x-get-resource "color2" "")))
      (setq org-todo-keyword-faces
	     `(("TODO" :foreground ,yellow :weight bold)
	       ("IN-PROGRESS" :foreground ,cyan :weight bold)
	       ("DONE" :foreground ,green :weight bold))))
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
