;;; package.el

;; Enable Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;;; Set up PATH
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;;; Basic Emacs settings

;; Startup
(use-package emacs
  :ensure nil
  :custom
  (inhibit-startup-screen t)
  :config
  (pixel-scroll-precision-mode)
  (scroll-bar-mode -1))

;; Don't clobber certain directories
(use-package emacs
  :ensure nil
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (load-file custom-file))

;; Backup files
(use-package emacs
  :ensure nil
  :custom
  ;; Avoid generating backups or lockfiles
  (create-lockfiles nil)
  (make-backup-files nil)
  ;; Configure backups
  (backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory))))
  (tramp-backup-directory-alist backup-directory-alist)
  (backup-by-copying t) ; Backup by copying rather than renaming
  (backup-by-copying-when-linked t)
  (delete-old-versions t) ; Delete excess backup versions silently
  (version-control t) ; Use version numbers for backup files
  (kept-new-versions 5)
  (kept-old-versions 5))

;; Disable bell
(use-package emacs
  :ensure nil
  :custom
  (visible-bell t)
  (ring-bell-function 'ignore))

(use-package evil
  :ensure t
  :custom
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (evil-set-leader 'motion (kbd "SPC"))
  (defun evil-leader-define-global (keybind function)
    (evil-define-key 'normal 'global (kbd keybind) function)))

(use-package evil-collection
  :ensure t
  :hook
  (after-init . evil-collection-init))

;; Imenu
(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t))

;; Direnv

(use-package direnv
  :ensure t
  :hook
  (after-init . direnv-mode))

;;; Minibuffer

(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not apply to the current mode
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-g i" . consult-imenu)
  ("C-x C-r" . consult-recent-file)
  :config
  (evil-leader-define-global "<leader>fs" #'consult-ripgrep)
  (evil-leader-define-global "<leader>fb" #'consult-buffer)
  (evil-leader-define-global "<leader>fi" #'consult-imenu)
  (evil-leader-define-global "<leader>fg" #'consult-goto-line))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode))

;;; Projects and workspaces
(use-package project
  :ensure nil
  :config
  (evil-leader-define-global "<leader>pp" #'project-switch-project)
  (evil-leader-define-global "<leader>pf" #'project-find-file)
  (evil-leader-define-global "<leader>pb" #'project-switch-to-buffer))

(use-package emacs
  :ensure nil
  :config
  (setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs-groups tab-bar-separator tab-bar-format-add-tab)))

(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window))

;;; User interface

(use-package emacs
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode) ; Show line numbers
  :custom
  (idle-update-delay 1.0) ; Update the UI a bit slower
  (use-short-answers t) ; use "y"/"n" instead of "yes"/"no"
  :config
  ;; Configure fonts
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140))

(use-package catppuccin-theme
  :ensure t
  :custom
  (catppuccin-flavor 'macchiato)
  :config
  (load-theme 'catppuccin t))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; Indent and formatting

(use-package emacs
  :ensure nil
  :custom
  ;; Indentation
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (comment-empty-lines t)
  (tab-stop-list (number-sequence 4 200 4))
  ;; (indent-line-function 'insert-tab)
  ;; Kill ring
  (kill-do-not-save-duplicates t))

;;; Repeat-mode

(use-package emacs
  :ensure nil
  :config
  (repeat-mode))

(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

(use-package smerge-mode
  :ensure nil
  :config
  (repeatize 'smerge-basic-map))

(defvar-keymap flymake-repeat-map
  :repeat t
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error
  "M-n" #'flymake-goto-next-error
  "M-p" #'flymake-goto-prev-error)

(use-package flymake
  :ensure nil
  ;; :custom
  ;; (flymake-show-diagnostics-at-end-of-line nil)
  :bind
  (:map flymake-mode-map
        ("M-p" . flymake-goto-prev-error)
        ("M-n" . flymake-goto-next-error)))

(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)


;;; Editor

(use-package emacs
  :ensure nil
  :config
  (delete-selection-mode))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-char-timer)
  ("C-:" . avy-goto-char))

;; Completion

(use-package corfu
  :ensure t
  :custom
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))

;;; Snippets

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

;;; Notes
(use-package org
  :ensure nil
  :hook
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  :custom
  (org-imenu-depth 7)
  (org-confirm-babel-evaluate nil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))

(use-package org-download
  :ensure t
  :hook
  (dired-mode . org-download-enable)
  (org-mode . org-download-enable))

(use-package howm
  :disabled
  :ensure t
  :init
  (require 'howm-org)
  :custom
  (howm-directory "~/Documents/howm")
  (howm-follow-theme t))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))


;;; Version control

(use-package magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :hook
  (after-init . global-diff-hl-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-show-hunk-function #'diff-hl-show-hunk-inline)
  :config
  (evil-define-key 'normal
    diff-hl-show-hunk-inline-transient-mode-map
    "q"
    #'diff-hl-show-hunk-inline-hide))

;; Programing

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nixd"))))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode)
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff)))

;; Tree-sitter
(use-package treesit
  :ensure nil
  :preface
  (defun ns/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               ;; (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               ;; (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :custom
  (treesit-font-lock-level 4)

  :config
  (ns/setup-install-grammars))

;;; Python

;; (use-package python-mode
;;   :ensure nil)
;; ;; :hook ((python-mode python-ts-mode) . eglot-ensure))

(use-package flymake-ruff
  :ensure t)
;; :hook ((python-mode python-ts-mode) . eglot-ensure))

;;; Nix
(use-package nix-mode
  :ensure t)
;; :hook
;; (nix-mode . eglot-ensure))

;;; PHP
(use-package php-ts-mode
  :ensure nil
  :mode "\\.php\\'")

;;; C
(use-package c-mode
  :ensure nil
  ;; :hook (c-mode . eglot-ensure)
  :custom
  (c-default-style "linux")
  (c-basic-offset 4))

;;; Yaml
(use-package yaml-ts-mode
  :ensure nil
  :preface
  (defun ns/setup-yaml ()
    (setq-local tab-width 2
                tab-stop-list (number-sequence 2 200 2)))
  :mode "\\.ya?ml\\'"
  :hook (yaml-ts-mode . ns/setup-yaml))

;;; Go
(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'")

;;; Haskell
(use-package haskell-mode
  :ensure t)


;; Terminal emulator

(use-package eat
  :ensure t)

(use-package vterm
  :ensure t
  :config
  (defun new-vterm ()
    "Create a new vterm session."
    (interactive)
    (call-interactively #'vterm)
    (rename-uniquely)))

;; Utilities

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t))

(use-package verb
  :ensure t
  :hook (org-mode . verb-mode)
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))
