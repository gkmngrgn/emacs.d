;;; ~/.emacs.d/init.el --- GKMNGRGN personal emacs configuration file.

;; Copyright (c) 2010-2020 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; Initial Setup
(setq-default cursor-type 'box)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(setq-default line-spacing 0)

(setq ring-bell-function 'ignore)
(setq require-final-newline t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq auto-window-vscroll nil)

;; Startup Settings
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)   ;; Show/hide startup page
(setq initial-scratch-message nil) ;; Show/hide *scratch* buffer message
(setq initial-major-mode 'text-mode)

;; Scroll Settings
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Mode Line Settings


;; Performance
(setq gc-cons-threshold 100000000)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook   'visual-line-mode)
(add-hook 'prog-mode-hook   'display-line-numbers-mode)

(menu-bar-mode 0)
(delete-selection-mode 1)
(global-hl-line-mode)
(global-auto-revert-mode)
(temp-buffer-resize-mode t)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment   'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Place all backup files in one directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory "~/")

;; Custom variables
(defvar custom-file-path "~/.emacs.d/custom.el")
(setq custom-file custom-file-path)
(when (file-exists-p custom-file-path)
  (load custom-file))

;; Packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; GUI settings
(if (display-graphic-p)
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      (set-frame-font "IBM Plex Mono Italic")
      (set-face-attribute 'default nil :height 130)))

;; Package configurations
(use-package avy
  :bind (("M-g g" . avy-goto-char-2)
         ("M-g f" . avy-goto-char))
  :config
  (avy-setup-default)
  :ensure t)

(use-package ace-window
  :config
  (custom-set-faces
   '(aw-leading-char-face ((t (:height 1.0)))))
  :defer t
  :diminish
  :ensure t
  :init
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-background nil)
  (setq aw-dispatch-always t))

(use-package ag
  :ensure t)

(use-package company
  :bind ("C-c TAB" . company-complete-common)
  :config
  (push 'company-capf company-backends)
  :diminish
  :ensure t
  :hook ((after-init . global-company-mode))
  :init
  (setq-default company-dabbrev-ignore-case t
                company-dabbrev-code-ignore-case t)
  (setq company-idle-delay 1
        company-minimum-prefix-length 2
        company-require-match 'never
        company-show-numbers nil
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-limit 10))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode t)
  :defer t
  :ensure t)

(use-package company-solidity
  :after solidity-mode
  :config
  (push 'company-solidity company-backends)
  :defer t
  :ensure t)

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-r"     . counsel-git-grep)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-d" . counsel-git))
  :ensure t)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (if (not (display-graphic-p))
      (diff-hl-margin-mode))
  :ensure t)

(use-package diminish
  :ensure t)

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (setq editorconfig-exclude-modes
        '(common-lisp-mode
          emacs-lisp-mode
          lisp-mode))
  (editorconfig-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :commands (exec-path-from-shell-initialize)
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind (("C-M-w" . er/expand-region))
  :defer t
  :ensure t)

(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t))

(use-package flycheck
  :diminish
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package focus
  :defer t
  :ensure t)

(use-package frog-jump-buffer
  :bind (("C-x C-b" . frog-jump-buffer))
  :defer t
  :ensure t)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :diminish
  :ensure t)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode t)
  :defer t
  :ensure t)

(use-package lsp-ivy
  :after counsel
  :commands (lsp-ivy-workspace-symbol)
  :defer t
  :ensure t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :diminish
  :ensure t
  :hook ((css-mode        . lsp-deferred)
         (dart-mode       . lsp-deferred)
         (go-mode         . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (python-mode     . lsp-deferred)
         (rust-mode       . lsp-deferred)
         (web-mode        . lsp-deferred)
         (yaml-mode       . lsp-deferred)
         (lsp-mode        . lsp-enable-which-key-integration))
  :init
  (custom-set-variables
   '(lsp-pyls-plugins-jedi-completion-enabled t)
   '(lsp-pyls-plugins-pyflakes-enabled t)
   '(lsp-pyls-plugins-mccabe-enabled nil)
   '(lsp-pyls-plugins-pylint-enabled nil)
   '(lsp-pyls-plugins-pycodestyle-enabled nil)
   '(lsp-pyls-plugins-pydocstyle-enabled nil)
   '(lsp-pyls-plugins-rope-completion-enabled nil)
   '(lsp-pyls-plugins-autopep8-enabled nil)
   '(lsp-pyls-plugins-yapf-enabled nil)
   '(lsp-pyls-plugins-flake8-enabled nil))
  (setq
   lsp-keymap-prefix "C-c l"
   lsp-prefer-capf t
   lsp-idle-delay 0.500
   lsp-enable-snippet nil                              ; company is better
   lsp-signature-doc-lines 10
   lsp-signature-auto-activate nil)
  (setq-default read-process-output-max (* 1024 1024)  ; 1mb
                lsp-rust-server 'rust-analyzer
                lsp-prefer-flymake nil))               ; flycheck is better

(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list)
  :defer t
  :ensure t)

(use-package lsp-ui
  :defer t
  :ensure t
  :commands (lsp-ui-mode)
  :init
  (setq lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 15))

(use-package magit
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t
  :config
  (set-face-attribute 'mode-line nil
                      :box nil
                      :height 110)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :inherit 'mode-line))

(use-package org
  :init
  (setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE")))
  (setq org-log-done t))

(use-package prescient
  :commands (prescient-persist-mode)
  :config
  (prescient-persist-mode t)
  :defer t
  :ensure t)

(use-package projectile
  :ensure t
  :diminish
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :init
  (setq projectile-completion-system 'ivy))

(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook prog-mode)

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :ensure t)

(use-package slime-company
  :ensure t)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company))
  :diminish
  :ensure t)

(use-package solidity-mode
  :defer t
  :ensure t)

(use-package solidity-flycheck
  :after solidity-mode
  :defer t
  :ensure t)

(use-package swiper
  :bind (("C-s" . swiper))
  :ensure t)

(use-package undo-fu
  :bind (("C-z" . undo-fu-only-undo)
         ("C-M-z" . undo-fu-only-redo))
  :defer t
  :ensure t)

(use-package unfill
  :bind (("C-M-q" . unfill-paragraph))
  :defer t
  :ensure t)

(use-package which-key
  :config
  (which-key-mode)
  :diminish
  :ensure t)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :diminish (yas-minor-mode)
  :ensure t)

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(use-package zoom
  :diminish
  :ensure t
  :config
  (zoom-mode t)
  (setq zoom-size '(0.618 . 0.618)
	zoom-ignored-major-modes '(which-key-mode
                                   hydra-mode)))

;; File modes
(use-package bazel-mode
  :defer t
  :ensure t)

(use-package cargo
  :after rust-mode
  :defer t
  :diminish (cargo-minor-mode)
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package dart-mode
  :defer t
  :ensure t)

(use-package dockerfile-mode
  :defer t
  :ensure t)

(use-package go-mode
  :defer t
  :ensure t)

(use-package lisp-mode
  :defer t
  :diminish eldoc-mode)

(use-package markdown-mode
  :defer t
  :ensure t
  :mode (("\\.md$" . markdown-mode))
  :config
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil
                      :foreground "dim gray")
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package powershell
  :defer t
  :ensure t)

(use-package rust-mode
  :defer t
  :ensure t)

(use-package scss-mode
  :defer t
  :ensure t
  :mode (("\\.scss$" . scss-mode)
         ("\\.sass$" . scss-mode)))

(use-package text-mode
  :diminish (visual-line-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\`/tmp/neomutt-" . mail-mode)))

(use-package vue-mode
  :defer t
  :ensure t
  :init
  (add-hook 'vue-mode-hook (lambda () (setq syntax-ppss-table nil)))
  (add-hook 'mmm-mode-hook
            (lambda ()
              (set-face-background 'mmm-default-submode-face nil))))

(use-package web-beautify
  :defer t
  :ensure t)

(use-package web-mode
  :defer t
  :ensure t
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-engines-alist '(("django" . "\\.html$"))
        web-mode-enable-auto-pairing nil
        web-mode-block-padding 0
        web-mode-enable-auto-indentation nil))

(use-package yaml-mode
  :defer t
  :ensure t)

;; Hydra settings
(use-package hydra
  :bind (("C-c e" . hydra-errors/body)
         ("C-c f" . hydra-focus/body))
  :config
  (with-no-warnings ;; to ignore the warning message "the following functions might not be defined..."
    (defhydra hydra-errors (:pre (flycheck-list-errors)
                                 :post (quit-windows-on "*Flycheck errors*")
                                 :hint nil)
      "Errors"
      ("f"   flycheck-error-list-set-filter      "Filter")
      ("j"   flycheck-next-error                 "Next")
      ("k"   flycheck-previous-error             "Previous")
      ("RET" nil                                 "Close" :color blue))

    (defhydra hydra-focus (:columns 4)
      "Focus"
      ("+"   text-scale-increase                 "Zoom in")
      ("-"   text-scale-decrease                 "Zoom out")
      ("f"   focus-mode                          "Focus")
      ("r"   focus-read-only-mode                "Review")

      ("g"   diff-hl-diff-goto-hunk              "Show diff")
      ("u"   diff-hl-revert-hunk                 "Revert diff")
      ("j"   diff-hl-next-hunk                   "Next diff")
      ("k"   diff-hl-previous-hunk               "Previous diff")

      ("s"   yas-insert-snippet                  "Insert snippet")
      ("n"   yas-new-snippet                     "New snippet")
      ("v"   yas-visit-snippet-file              "Visit snippet")
      ("RET" nil                                 "Close" :color blue)))
  :ensure t)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
