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
 '(custom-enabled-themes (quote (wombat)))
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
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;;; All text modes (also AucTeX)
;; Enable auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

(use-package evil
  :init
  (progn
    (require 'evil)
    (evil-mode 1)))

(use-package linum-off
  :init
  (progn
    (require 'linum-off)))

(use-package hlinum
  :init
  (progn
    (require 'hlinum)
    (hlinum-activate)))
