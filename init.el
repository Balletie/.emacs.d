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
 '(aggressive-indent-excluded-modes
   (quote
    (inf-ruby-mode makefile-mode makefile-gmake-mode python-mode text-mode yaml-mode nix-mode haskell-mode)))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "firefox")
 '(cmake-ide-flags-c
   "-I/nix/store/psmdlfqys1031hhyjhky4qphgyscmgdg-gcc-5.4.0/include/c++/5.4.0")
 '(cmake-ide-flags-c++
   "-I/nix/store/psmdlfqys1031hhyjhky4qphgyscmgdg-gcc-5.4.0/include/c++/5.4.0")
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "92f826e9660492a15f363891cd9c128eb381ebe2ba1efe804224c895e742bfa2" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "cdbd2c07cda87348734c588b9d6d300af4e073f7b8158ab1dfba00be84b37ca6" "a1ea1b279f80fdb7868418563bd417faf144ad21dda217a17c52548606adcdd7" default)))
 '(debug-on-error nil)
 '(doc-view-continuous t)
 '(doc-view-pdfdraw-program "mutool draw")
 '(evil-emacs-state-modes
   (quote
    (archive-mode bbdb-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode etags-select-mode exwm-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode git-commit-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-cherry-mode magit-diff-mode magit-log-mode magit-log-select-mode magit-popup-mode magit-popup-sequence-mode magit-process-mode magit-reflog-mode magit-refs-mode magit-revision-mode magit-stash-mode magit-stashes-mode magit-status-mode magit-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode)))
 '(evil-want-C-u-scroll t)
 '(fci-rule-color "#424242")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(fringe-mode nil nil (fringe))
 '(gdb-many-windows t)
 '(global-linum-mode t)
 '(haskell-indent-spaces 4)
 '(helm-external-programs-associations (quote (("html" . "firefox"))))
 '(horizontal-scroll-bar-mode nil)
 '(linum-disabled-modes-list
   (quote
    (eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode pdf-view-mode doc-view-mode)))
 '(magit-diff-refine-hunk t)
 '(magit-popup-use-prefix-argument nil)
 '(markdown-preview-style "http://kevinburke.bitbucket.org/markdowncss/markdown.css")
 '(menu-bar-mode nil)
 '(nix-nixpkgs-path nil)
 '(org-agenda-files (quote ("~/Documents/Org/todo.org")))
 '(org-babel-load-languages
   (quote
    ((shell . t)
     (python . t)
     (ruby . t)
     (emacs-lisp . t))))
 '(org-babel-lob-files (quote ("/home/skip/Pictures/amaravati/album.org")))
 '(org-babel-python-command "python3")
 '(org-checkbox-hierarchical-statistics t)
 '(org-default-notes-file "~/Documents/Org/todo.org")
 '(org-export-with-smart-quotes t)
 '(org-export-with-statistics-cookies t)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "firefox %s")
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
 '(password-cache-expiry 60)
 '(reftex-plug-into-AUCTeX t)
 '(rm-blacklist (quote (" hl-p" " Undo-Tree" " …")))
 '(safe-local-variable-values
   (quote
    ((org-duration-format . h:mm)
     (org-trello-files quote
		       ("./trello.org"))
     (cmake-ide-build-dir . "bin")
     (org-export-babel-evaluate quote inline-only)
     (org-confirm-babel-evaluate)
     (org-refile-targets
      ("/home/skip/Programming/Spoofax/bep-spoofax-repl/archive.org" :level . 1))
     (org-refile-targets
      ("/home/skip/Programming/Spoofax/bep-spoofax-repl/worklog.org" :level . 1))
     (TeX-master . main)
     (org-refile-targets quote
			 (("worklog.org" :level . 0)))
     (TeX-engine . xelatex))))
 '(savehist-mode t)
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
 '(evil-goggles-delete-face ((t (:inherit (quote diff-removed)))))
 '(evil-goggles-paste-face ((t (:inherit (quote diff-added)))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit (quote diff-refine-added)))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit (quote diff-refine-removed)))))
 '(evil-goggles-yank-face ((t (:inherit (quote diff-changed))))))

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

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Use M-x benchmark-init/show-durations-tree to show benchmark results.
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Colorize compilation output (http://stackoverflow.com/a/3072831)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package direnv
  :diminish direnv-mode
  :config
  (setq direnv-always-show-summary t
	direnv-show-paths-in-summary nil)
  (direnv-mode))

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

(use-package flycheck
  :defer t)

(use-package company
  :diminish company-mode
  :bind (([M-tab] . company-complete))
  :demand t
  :config
  (global-company-mode))

(use-package company-jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (defun python-add-company-backend ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'python-add-company-backend))

(use-package c++-mode
  :mode ("\\.tcc\\'"
	 "\\.tpp\\'"))

(use-package php-mode
  :defer t)

(use-package web-mode
  :mode ("\\.html?\\'"
	 "\\.css\\'"
	 "\\.phtml\\'"
	 "\\.php\\'"
	 "\\.[agj]sp\\'"
	 "\\.as[cp]x\\'"
	 "\\.erb\\'"
	 "\\.mustache\\'"
	 "\\.djhtml\\'"))

(use-package haskell-mode
  :mode ("\\.hs\\'"
	 "\\.lhs\\'"))
(use-package dante
  :after haskell-mode
  :defer t
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'dante-mode-hook
	    '(lambda () (flycheck-add-next-checker 'haskell-dante
						   '(warning . haskell-hlint)))))

(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
	projectile-completion-system 'helm
	projectile-switch-project-action 'helm-projectile)
  (helm-projectile-on)
  (projectile-global-mode))

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
  (require 'ox-bibtex)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "DONE")))
  (setq org-todo-keyword-faces
	'(("IN-PROGRESS" org-ellipsis)))
  (setq org-capture-templates
	'(("t" "Todo" entry (file org-default-notes-file)
	   "* TODO %? %^g")))
  (setq org-ellipsis "…")
  ;; Enable indent mode
  (add-hook 'org-mode-hook 'org-indent-mode))
;; Org-mode agenda key.
(global-set-key "\C-ca" 'org-agenda)

(use-package org-bullets
  :defer t
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-beautify
  :defer t )

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
  (setq git-gutter-fr:side 'right-fringe
	git-gutter:update-interval 2)
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

(use-package doc-view
  :defer t
  :bind (:map doc-view-mode-map
	      ("j" . doc-view-scroll-up-or-next-page)
	      ("k" . doc-view-scroll-down-or-previous-page)
	      ("h" . image-backward-hscroll)
	      ("l" . image-forward-hscroll)
	      ("<down>" . doc-view-next-page)
	      ("<up>" . doc-view-previous-page))
  :init
  (set-window-fringes (selected-window) 0 0))

(defvar my-fixed-font "Iosevka" "Fixed width font for programming modes")
(set-frame-font my-fixed-font)
(set-face-attribute 'default nil :family my-fixed-font :height 100)

(deftheme my-org-theme "Subtheme for org-mode")

(defun my-change-theme ()
  (let ((headline-face '(default))
	(primary-color (face-foreground 'mode-line))
	(highlight-bg (face-background 'secondary-selection nil 'region))
	(low-contrast-bg (face-background 'linum)))
    (custom-theme-set-faces 'my-org-theme
			    `(org-agenda-structure ((t (,@headline-face :height 2.0 :underline nil))))
			    `(org-level-8 ((t (:inherit (outline-8 ,@headline-face)))))
			    `(org-level-7 ((t (:inherit (outline-7 ,@headline-face)))))
			    `(org-level-6 ((t (:inherit (outline-6 ,@headline-face)))))
			    `(org-level-5 ((t (:inherit (outline-5 ,@headline-face)))))
			    `(org-level-4 ((t (:inherit (outline-4 ,@headline-face)))))
			    `(org-level-3 ((t (:inherit (outline-3 ,@headline-face)))))
			    `(org-level-2 ((t (:inherit (outline-2 ,@headline-face) :height 1.25))))
			    `(org-level-1 ((t (:inherit (outline-1 ,@headline-face) :height 1.5 :background ,highlight-bg))))
			    `(org-ellipsis ((t (:box (:line-width 1 :color ,highlight-bg) :foreground ,primary-color :background ,low-contrast-bg))))
			    `(org-document-title ((t (:inherit org-level-1 :height 1.25 :underline nil))))
			    `(org-checkbox ((t (:foreground ,primary-color :background ,highlight-bg :weight bold :box (:line-width 1 :style released-button))))))))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme--after-load-theme-run-hook (&optional args)
  (run-hooks 'after-load-theme-hook))

(advice-add 'load-theme :after 'load-theme--after-load-theme-run-hook)

(add-hook 'after-load-theme-hook #'my-change-theme)

(color-theme-sanityinc-tomorrow-blue)
