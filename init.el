; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(set-language-environment "utf-8")

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
 '(browse-url-browser-function (quote browse-url-generic))
 '(cmake-ide-flags-c
   "-I/nix/store/psmdlfqys1031hhyjhky4qphgyscmgdg-gcc-5.4.0/include/c++/5.4.0")
 '(cmake-ide-flags-c++
   "-I/nix/store/psmdlfqys1031hhyjhky4qphgyscmgdg-gcc-5.4.0/include/c++/5.4.0")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "92f826e9660492a15f363891cd9c128eb381ebe2ba1efe804224c895e742bfa2" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "cdbd2c07cda87348734c588b9d6d300af4e073f7b8158ab1dfba00be84b37ca6" "a1ea1b279f80fdb7868418563bd417faf144ad21dda217a17c52548606adcdd7" default)))
 '(debug-on-error nil)
 '(evil-want-C-u-scroll t)
 '(fci-rule-color "#424242")
 '(fringe-mode nil nil (fringe))
 '(gdb-many-windows t)
 '(global-linum-mode t)
 '(horizontal-scroll-bar-mode nil)
 '(magit-diff-refine-hunk t)
 '(magit-popup-use-prefix-argument nil)
 '(markdown-preview-style "http://kevinburke.bitbucket.org/markdowncss/markdown.css")
 '(menu-bar-mode nil)
 '(nix-nixpkgs-path nil)
 '(org-agenda-files
   (quote
    ("~/Programming/Spoofax/bep-spoofax-repl/trello.org" "~/Programming/Spoofax/bep-spoofax-repl/user-stories.org")))
 '(org-babel-load-languages (quote ((shell . t) (python . t) (ruby . t))))
 '(org-babel-lob-files (quote ("/home/skip/Pictures/amaravati/album.org")))
 '(org-babel-python-command "python3")
 '(org-checkbox-hierarchical-statistics t)
 '(org-default-notes-file "~/Documents/Org/todo.org")
 '(org-ellipsis "⤵")
 '(org-export-with-smart-quotes t)
 '(org-export-with-statistics-cookies t)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "surf %s")
     ("\\.pdf\\'" . default))))
 '(org-html-mathjax-options
   (quote
    ((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML")
     (scale "100")
     (align "center")
     (font "TeX")
     (linebreaks "false")
     (autonumber "AMS")
     (indent "0em")
     (multlinewidth "85%")
     (tagindent ".8em")
     (tagside "right"))))
 '(org-latex-classes
   (quote
    (("article-shifted" "\\documentclass[11pt]{article}"
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-latex-prefer-user-labels t)
 '(org-refile-use-outline-path (quote file))
 '(reftex-plug-into-AUCTeX t)
 '(rm-blacklist (quote (" hl-p" " Undo-Tree" " …")))
 '(safe-local-variable-values
   (quote
    ((org-export-babel-evaluate quote inline-only)
     (org-confirm-babel-evaluate)
     (org-refile-targets
      ("/home/skip/Programming/Spoofax/bep-spoofax-repl/archive.org" :level . 1))
     (org-refile-targets
      ("/home/skip/Programming/Spoofax/bep-spoofax-repl/worklog.org" :level . 1))
     (TeX-master . main)
     (org-refile-targets quote
			 (("worklog.org" :level . 0)))
     (TeX-engine . xelatex))))
 '(scroll-bar-mode nil)
 '(sml/replacer-regexp-list
   (quote
    (("^~/Documents/Org/" ":Org:")
     ("^~/\\.emacs\\.d/elpa/" ":ELPA:")
     ("^~/\\.emacs\\.d/" ":ED:")
     ("^/sudo:.*:" ":SU:")
     ("^~/Documents/" ":Doc:")
     ("^~/Programming/" ":Prog:"))))
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
(setq debug-on-message "^Wrong")

;; Uncomment and use M-x benchmark-init/show-durations-tree to benchmark.
;(require 'benchmark-init)
;(benchmark-init/activate)

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

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

(use-package nix-sandbox
  :commands (nix-current-sandbox nix-shell-command nix-executable-find))

(use-package pretty-sha-path
  :init
  (autoload 'pretty-sha-path-mode "pretty-sha-path" nil t)
  (autoload 'global-pretty-sha-path-mode "pretty-sha-path" nil t)
  :config
  (global-pretty-sha-path-mode))

(use-package rtags
  :defer t
  :commands rtags-find-symbol-at-point)

(use-package flycheck
  :defer t
  :init
  (progn
    (setq flycheck-command-wrapper-function
	  (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
	  flycheck-executable-find
	  (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))
    (add-hook 'c++-mode-hook 'flycheck-mode)
    (add-hook 'c-mode-hook 'flycheck-mode)))

(use-package company
  :diminish company-mode
  :bind (([M-tab] . company-complete))
  :init
  (progn
    (defun set-sandbox-clang()
      "Sets the clang executable to the one of the sandbox"
      (setq company-clang-executable (nix-executable-find (nix-current-sandbox) "clang"))
      (make-local-variable 'company-clang-executable))
    (add-hook 'c-mode-hook #'set-sandbox-clang)
    (add-hook 'c++-mode-hook #'set-sandbox-clang))
  :config
  (global-company-mode))

(use-package c++-mode
  :mode "\\.tcc\\'")

(use-package cmake-ide
  :commands (cmake-ide-compile)
  :init
  (defun set-sandbox-commands()
    "Sets the make and cmake commands so that they run those of the nix-sandbox."
    (set (make-local-variable 'cmake-ide-rdm-executable) (nix-executable-find (nix-current-sandbox) "rdm"))
    (set (make-local-variable 'cmake-ide-cmake-command) (nix-executable-find (nix-current-sandbox) "cmake"))
    (set (make-local-variable 'cmake-ide-make-command) (nix-executable-find (nix-current-sandbox) "make")))
  (add-hook 'c-mode-hook #'set-sandbox-commands)
  (add-hook 'c++-mode-hook #'set-sandbox-commands)
  (setq cmake-ide-command-wrapper-function
	(lambda (command) (apply 'nix-shell-command
				 (cl-some 'nix-find-sandbox (append (last command) `(,default-directory)))
				 command)))
  :config
  (cmake-ide-setup))

(use-package helm
  :diminish helm-mode
  :bind (("M-x"     . helm-M-x)
	 ("C-x b"   . helm-mini)
	 ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1))

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
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
  (add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "latexmk"))))

(use-package org
  :defer t
  :config
  (progn
    (require 'ox-bibtex)
    (setq org-todo-keywords
	  '((sequence "TODO" "IN-PROGRESS" "DONE")))
    (setq org-todo-keyword-faces
	  '(("IN-PROGRESS" org-ellipsis)))
    (setq org-capture-templates
	  '(("t" "Todo" entry (file org-default-notes-file)
	     "* TODO %? %^g")))
    ;; Enable indent mode
    (add-hook 'org-mode-hook 'org-indent-mode)))
;; Org-mode agenda key.
(global-set-key "\C-ca" 'org-agenda)

(use-package evil
  :init
  (progn
    (require 'evil)
    (evil-mode 1)))

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status) ("C-x G" . magit-dispatch-popup)))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode)

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :init
  (require 'git-gutter-fringe)
  (setq git-gutter-fr:side 'right-fringe)
  :config
  (global-git-gutter-mode))

(use-package linum-off
  :init
  (progn
    (require 'linum-off)))

(use-package hlinum
  :init
  (progn
    (require 'hlinum)
    (hlinum-activate)))

(use-package smart-mode-line
  :config
  (sml/setup)
  (display-time-mode 1)
  (display-battery-mode 1)
  (setq display-time-24hr-format t
	display-time-day-and-date t
	display-time-default-load-average nil
	display-time-load-average-threshold 100 ; Really high, I don't want to ever see it.
	battery-mode-line-limit 1000
	battery-update-interval 5
	sml/battery-format " %p%% "))

(use-package avy
  :bind (("C-," . avy-goto-char-2)
	 ("C-;" . avy-goto-word-1)
	 ("C-'" . avy-isearch)))

(use-package popwin
  :init
  (require 'popwin)
  (popwin-mode 1))
