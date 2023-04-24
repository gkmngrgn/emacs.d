;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; INITIALIZE
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

(setq package-archive-priorities '(("melpa" . 5) ("jcs-elpa" . 0)))

(package-initialize)

;; EMACS
(use-package emacs
  :init
  (load-theme 'modus-vivendi :no-confirm)
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . visual-line-mode))
  :custom (modus-themes-mode-line '(borderless accented))
  :bind (("<mouse-4>" . scroll-down-line)
         ("<mouse-5>" . scroll-up-line)
         ("C-c SPC" . comment-line)
         ("C-x C-b" . ibuffer)
         ("C-z" . undo-only)))

(use-package completion
  :custom ((completion-auto-help t)
           (completion-auto-select 'second-tab)
           (completion-cycle-threshold 3)
           (completions-max-height 10)
           (completions-header-format nil)
           (tab-always-indent 'complete)))

;; PACKAGES
(use-package chatgpt :ensure t)

(use-package codegpt :ensure t)

(use-package copilot
  :ensure t
  :bind ("C-c g i" . copilot-accept-completion))

(use-package expand-region
  :ensure t
  :bind ("C-c w" . er/expand-region))

(use-package eglot
  :bind (("C-c l l" . eglot)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)))

(use-package find-file-in-project
  :ensure t
  :custom ((ffip-prefer-ido-mode t)
           (ffip-use-rust-fd t))
  :bind ("C-c f" . ffip))

(use-package flymake
  :bind (("C-c l n" . flymake-goto-next-error)
         ("C-c l p" . flymake-goto-prev-error)))

(use-package magit
  :ensure t
  :config
  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)))

(use-package minions
  :ensure t
  :config (minions-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m" . mc/edit-lines)
         ("C-c j" . mc/mark-next-like-this)
         ("C-c k" . mc/mark-previous-like-this)
         ("C-c n" . mc/mark-all-like-this)))

(use-package openai
  :ensure t
  :custom (openai-key (getenv "OPENAI_KEY")))

(use-package poetry
  :ensure t)

(use-package pyenv-mode
  :ensure t
  :config (pyenv-mode))

(use-package rg
  :ensure t
  :config (rg-enable-default-bindings))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config (global-treesit-auto-mode))

;; FILE MODES
(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-command "multimarkdown"))

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
