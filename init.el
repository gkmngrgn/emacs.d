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
(defun goedev/switch-to-previous-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(defun goedev/switch-to-default-buffer-group ()
  "Switch to default buffer group."
  (ibuffer-switch-to-saved-filter-groups "default"))

(use-package emacs
  :init
  (if (display-graphic-p)
      (progn
        (defvar my-font "Jetbrains Mono")
        (defvar my-font-size 150)
        (set-frame-font my-font)
        (set-face-attribute 'default nil :height my-font-size :font my-font)
        (set-face-attribute 'fixed-pitch nil :height my-font-size :font my-font)
        (fringe-mode 0)
        (tool-bar-mode 0)))
  (load-theme 'modus-vivendi :no-confirm)
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . visual-line-mode)
         (ibuffer-mode . goedev/switch-to-default-buffer-group))
  :custom ((completion-cycle-threshold 3)
           (tab-always-indent 'complete)
           (modus-themes-mode-line '(borderless accented)))
  :bind (("<mouse-4>" . scroll-down-line)
         ("<mouse-5>" . scroll-up-line)
         ("C-c SPC" . comment-line)
         ("C-c o" . goedev/switch-to-previous-window)
         ("C-x C-b" . ibuffer)
         ("C-z" . undo-only)))

;; PACKAGES
(use-package chatgpt :ensure t)

(use-package codegpt :ensure t)

(use-package copilot
  :ensure t
  :bind ("C-c g i" . copilot-accept-completion))

(use-package corfu
  :ensure t
  :init (global-corfu-mode))

(use-package corfu-terminal
  :ensure t
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

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

(use-package ido
  :custom (ido-everywhere t)
  :config (ido-mode t))

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
