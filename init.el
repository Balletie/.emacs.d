;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    ;; (package-initialize)
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 0))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "usr" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package dash)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (registers . 5)))
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-page-separator "\n\f\n")
  (dashboard-set-footer nil))

(use-package diminish)

(use-package eieio)

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package no-littering
  :demand t)

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

(progn ; `map-ynp'
  ;; Make all "yes or no" prompts show "y or n" instead
  (setq read-answer-short t)
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package modus-themes
  :demand t
  :bind ("<f5>" . modus-themes-toggle)
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-completions '((matches . (extrabold background intense))
                                   (selection . (semibold accented intense))
                                   (popup . (accented)))
        modus-themes-org-blocks 'greyscale
        modus-themes-common-palette-overrides '((border-mode-line-active unspecified)
                                                (border-mode-line-inactive unspecified)))
  :config
  (declare-function modus-themes-load-theme "modus-themes.el")
  (modus-themes-load-theme 'modus-vivendi))

(use-package frame
  :config
  (setq frame-title-format "Emacs (%b)")
  (set-face-attribute 'default nil :family "Iosevka Term" :width 'regular :height 120)
  (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
  (set-face-attribute 'variable-pitch nil :family "Cantarell"))

(use-package moody
  :demand t
  :config
  ;; Adapted from Damien's init.el
  (defvar my/mode-line-mule-info
    '(:eval (unless (eq buffer-file-coding-system
                        (default-value 'buffer-file-coding-system))
              (list mode-line-mule-info " "))))
  (put 'my/mode-line-mule-info 'risky-local-variable t)
  (make-local-variable 'my/mode-line-mule-info)

  (defvar my/mode-line-position
    (list mode-line-percent-position " %l:%c "))

  (defvar my/mode-line-buffer-identification
    '(:eval (moody-tab (let* ((mode-line (propertized-buffer-identification "%b"))
                              (mode-line (format-mode-line mode-line)))
                         (when (and buffer-file-name buffer-read-only)
                           (add-face-text-property 0 (length mode-line) '(:foreground "red")
                                                   nil mode-line))
                         (when (buffer-modified-p (current-buffer))
                           (add-face-text-property 0 (length mode-line) '(:slant italic)
                                                   nil mode-line))
                         mode-line)
                       20 'down)))
  (put 'my/mode-line-buffer-identification 'risky-local-variable t)
  (make-local-variable 'my/mode-line-buffer-identification)

  (setq x-underline-at-descent-line t)

  ;(moody-replace-mode-line-buffer-identification)
  (declare-function moody-replace-element "moody.el")
  (moody-replace-element 'mode-line-mule-info 'my/mode-line-mule-info)
  (moody-replace-element 'mode-line-position 'my/mode-line-position)
  (moody-replace-element 'mode-line-buffer-identification 'my/mode-line-buffer-identification)
  (moody-replace-vc-mode)

  (setq mode-line-format (delq 'mode-line-remote mode-line-format))
  (setq mode-line-format (delq 'mode-line-modified mode-line-format))
  (setq mode-line-format (delq 'mode-line-client mode-line-format)))

;;; Long tail

(use-package avy
  :bind (("C-;" . avy-goto-word-1)
	 ("C-'" . avy-isearch)))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package caddyfile-mode
  :mode (("CaddyFile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

(use-package consult
  :demand
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s a" . consult-ag)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package devdocs
  :bind (("C-h D" . devdocs-lookup)))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package diff-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package direnv
  :defer t
  :config
  (setq direnv-always-show-summary t
	direnv-show-paths-in-summary nil)
  (declare-function direnv--maybe-update-environment "direnv.el")
  (add-hook 'prog-mode-hook #'direnv--maybe-update-environment)
  (direnv-mode))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

(use-package dpkg-dev-el)

(use-package eglot
  :defer t
  :hook ((python-mode python-ts-mode) . eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:plugins (
                                       :pycodestyle (:enabled :json-false)
                                       :mccabe (:enabled :json-false)
                                       :pyflakes (:enabled :json-false)
                                       :flake8 (:enabled :json-false)
                                       :ruff (:enabled t)
                                       :yapf (:enabled :json-false)
                                       :autopep8 (:enabled :json-false)
                                       :black (:enabled :json-false)))))))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package embark
  :demand t
  :bind
  (("C-." . embark-act)
   ("C-," . embark-select)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :demand t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package git-annex)

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package jinja2-mode
  :defer t
  :config
  (setq indent-tabs-mode nil))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode))

(use-package magit
  :defer t
  :commands (magit-add-section-hook)
  :bind (("C-x g" . magit-status) ("C-x G" . magit-dispatch))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package magit-annex
  :after magit)

(use-package make-mode
  :hook
  (makefile-mode . indent-tabs-mode))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package marginalia
  :demand t
  :bind (
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (progn
    (marginalia-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package meow
  :diminish (meow-normal-mode meow-insert-mode meow-beacon-mode meow-motion-mode meow-keypad-mode)
  :custom
  (meow-expand-hint-counts '((word . 20)
                             (line . 0)
                             (block . 20)
                             (find . 20)
                             (till . 20)))
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore))
    (meow-setup-indicator)
    (meow-setup-line-number))
  (meow-setup)
  (meow-global-mode 1))

(use-package nix-mode
  :mode ("\\.nix\\'"))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package page-break-lines
  :custom
  (page-break-lines-char ?⋅)
  (page-break-lines-modes '(emacs-lisp-mode lisp-mode scheme-mode
                            compilation-mode outline-mode help-mode
                            makefile-mode))
  :config
  (global-page-break-lines-mode))

(use-package paren
  :config (show-paren-mode))

(use-package pet
  :config
  (add-hook 'python-ts-mode-hook
            #'(lambda ()
                ;; Python interpreter
                (setq-local python-shell-interpreter (pet-executable-find "python")
                            python-shell-virtualenv-root (pet-virtualenv-root))
                ;; Pytest (for python-pytest)
                ;;(setq-local python-pytest-executable (pet-executable-find "pytest"))
                ;; Eglot
                (require 'eglot)
                (setq-local eglot-server-programs
                            (cons `((python-mode python-ts-mode)
                                    . (,(pet-executable-find "pylsp")))
                                  eglot-server-programs)))))

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package prog-mode
  :config
  (defun indent-spaces-mode ()
    (setq-local indent-tabs-mode nil))
  (add-hook 'prog-mode-hook 'indent-spaces-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left))

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t)
  (projectile-mode))

(use-package recentf
  :demand t
  :init (recentf-mode 1)
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(use-package smerge-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'smerge-refined-removed nil :extend t)
    (set-face-attribute 'smerge-refined-added   nil :extend t)))

(use-package solaire-mode
  :config
  (solaire-global-mode 1))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook 'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package tramp-sh
  :defer t
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

(use-package vertico
  :config
  (vertico-mode))

(use-package which-key
  :demand
  :diminish
  :bind (("C-h K" . which-key-show-full-major-mode)
         ("C-h C-k" . which-key-show-top-level))
  :config
  (which-key-mode))

(use-package with-editor
  :commands (with-editor-export-editor with-editor-shell-command with-editor-async-shell-command)
  :init
  (shell-command-with-editor-mode)
  (dolist (hook '(shell-mode-hook term-exec-hook eshell-mode-hook))
    (dolist (envvar '("EDITOR" "GIT_EDITOR"))
      (add-hook hook (apply-partially #'with-editor-export-editor envvar)))))

(use-package web-mode
  :mode ("\\.html?\\'"
	 "\\.phtml\\'"
	 "\\.php\\'"
	 "\\.[agj]sp\\'"
	 "\\.as[cp]x\\'"
	 "\\.erb\\'"
	 "\\.mustache\\'"))

(use-package wgrep
  :demand t
  :after (grep)
  :init
  (progn
    (setq wgrep-enable-key [remap read-only-mode])
    (setq wgrep-auto-save-buffer t)))

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
