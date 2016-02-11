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
    (defvar cask-path "/usr/share/emacs/site-lisp/cask/cask.el"))
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
 '(safe-local-variable-values (quote ((TeX-engine . xelatex)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Set path for AucTeX
(getenv "PATH")
(setenv "PATH"
	(concat "/usr/texbin" ":" "/usr/local/bin" ":" "/usr/local/sbin" ":"
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

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "/usr/texbin/latexmk -pdf -e \'$pdflatex=q{/usr/texbin/pdflatex -synctex=1 %O %s.tex}\'" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))

(require 'server)
(unless (server-running-p)
  (server-start))
