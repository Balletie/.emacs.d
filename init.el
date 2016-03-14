(set-language-environment "utf-8")

;; ;;; Add repository
;; (require 'package)

;; (push '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
;; (push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)

;; (setq my-packages
;;       '(evil
;; 	magit
;; 	linum-off
;; 	hlinum
;; 	auctex))

;; (package-initialize)

;; (or (file-exists-p package-user-dir)
;;     (package-refresh-contents))

;; ;;; Install missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

(if (eq system-type 'darwin)
    (defvar cask-path "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (progn (defvar cask-path "/usr/share/emacs/site-lisp/cask/cask.el")
	 (unless (file-exists-p cask-path)
	   (setq cask-path "~/.nix-profile/cask.el"))))
(require 'cask cask-path)
(cask-initialize)

;;; Evil mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;;; Line numbers
(global-linum-mode 1)
(setq linum-format "%d ")
 ;; linum-off defines the modes where line numbers are turned off.
(require 'linum-off)
 ;; hlinum highlights the current line
(require 'hlinum)
(hlinum-activate)

;;; Tabs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(magit-diff-refine-hunk t)
 '(safe-local-variable-values (quote ((TeX-engine . xelatex))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;;; All text modes (also AucTeX
;; Enable auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; AucTeX
;; Set path for AucTeX
(getenv "PATH")
(setenv "PATH"
	(concat "/usr/bin/" ":" "/usr/texbin" ":" "/usr/local/bin" ":" "/usr/local/sbin" ":"
		(getenv "PATH")))
;;; From http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Make latexmk available via C-c C-c
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
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(defvar pdfviewer "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")
(unless (eq system-type 'darwin)
  (setq pdfviewer "mupdf-x11 %o"))
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     `(("PDF Viewer" ,pdfviewer)))

(require 'server)
(unless (server-running-p)
  (server-start))
