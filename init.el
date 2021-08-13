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
  (borg-initialize)
  (setq borg-rewrite-urls-alist
      '(("git@github.com:" . "https://github.com/")
        ("git@gitlab.com:" . "https://gitlab.com/"))))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package dash)
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
  :init
  (setq modus-themes-mode-line 'borderless-moody)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-completions 'opinionated)
  (setq modus-themes-org-blocks 'greyscale)

  (setq modus-themes-headings
        '((1 . highlight) ; make h1 stand out with a background
          (2 . line)      ; add a line above h2
          (t . rainbow))) ; choose a random color for all headings

  (setq modus-themes-scale-headings t)
  (setq modus-themes-slanted-constructs t)
  (setq modus-themes-variable-pitch-headings t)
  :config
  (modus-themes-load-operandi))

(use-package frame
  :config
  (setq frame-title-format "Emacs (%b)")
  (set-face-attribute 'default nil :family "Iosevka Term" :width 'regular :height 104)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Term" :width 'regular :height 104)
  (set-face-attribute 'variable-pitch nil :font "Cantarell"))

(use-package moody
  :demand t
  :config
  ;; Adapted from Damien's init.el
  (defvar my/mode-line-mule-info
    '(:eval (unless (eq buffer-file-coding-system
                        (default-value 'buffer-file-coding-system))
              (list mode-line-mule-info " "))))
  (put 'my/mode-line-mule-info 'risky-local-variable t)
  (make-variable-buffer-local 'my/mode-line-mule-info)

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
  (make-variable-buffer-local 'my/mode-line-buffer-identification)

  (setq x-underline-at-descent-line t)

  (setq evil-emacs-state-tag    (propertize " EMACS   " 'face 'modus-themes-refine-red)
        evil-insert-state-tag   (propertize " INSERT  " 'face 'modus-themes-refine-green)
        evil-replace-state-tag  (propertize " REPLACE " 'face 'modus-themes-refine-yellow)
        evil-normal-state-tag   " NORMAL  "
        evil-visual-char-tag    (propertize " VISUAL  " 'face 'modus-themes-refine-blue)
        evil-visual-line-tag    (propertize " V-LINE  " 'face 'modus-themes-refine-blue)
        evil-visual-block-tag   (propertize " V-BLOCK " 'face 'modus-themes-refine-blue)
        evil-operator-state-tag (propertize " PENDING " 'face 'modus-themes-refine-neutral))
  (setq evil-mode-line-format '(before . mode-line-front-space))

  ;(moody-replace-mode-line-buffer-identification)
  (moody-replace-element 'mode-line-mule-info 'my/mode-line-mule-info)
  (moody-replace-element 'mode-line-position 'my/mode-line-position)
  (moody-replace-element 'mode-line-buffer-identification 'my/mode-line-buffer-identification)
  (moody-replace-vc-mode)

  (setq mode-line-format (delq 'mode-line-remote mode-line-format))
  (setq mode-line-format (delq 'mode-line-modified mode-line-format))
  (setq mode-line-format (delq 'mode-line-client mode-line-format)))

;;; Long tail

(use-package avy
  :bind (("C-," . avy-goto-char-2)
	 ("C-;" . avy-goto-word-1)
	 ("C-'" . avy-isearch)))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package counsel-dash
  :bind (("C-x C-h d" . counsel-dash)
         ("C-x C-h i" . counsel-dash-at-point))
  :config
  (setq dash-docs-enable-debugging nil
        dash-docs-common-docsets (list "SQLite" "JavaScript" "Flask" "Sass" "svelte" "HTML" "Python 3")))

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
  (add-hook 'prog-mode-hook #'direnv--maybe-update-environment)
  (direnv-mode))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-goggles
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package ivy
  :diminish (ivy-mode counsel-mode)
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :demand t
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook 'indent-spaces-mode))

(use-package magit
  :defer t
  :commands (magit-add-section-hook)
  :bind (("C-x g" . magit-status) ("C-x G" . magit-dispatch))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

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

(use-package nix-mode
  :mode ("\\.nix\\'"))

(use-package nix-prettify-mode
  :diminish nix-prettify-mode
  :config
  (nix-prettify-global-mode))

(use-package pali)

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left))

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(use-package recentf
  :demand t
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

(use-package which-key
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
	 "\\.css\\'"
	 "\\.phtml\\'"
	 "\\.php\\'"
	 "\\.[agj]sp\\'"
	 "\\.as[cp]x\\'"
	 "\\.erb\\'"
	 "\\.mustache\\'"))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :init
  (yas-reload-all))

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
