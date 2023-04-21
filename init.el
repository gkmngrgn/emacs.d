;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; DEPENDENCIES
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

(setq package-archive-priorities '(("melpa" . 5) ("jcs-elpa" . 0)))

(package-initialize)

(use-package chatgpt :ensure t)

(use-package codegpt :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-c w" . er/expand-region))

(use-package eglot
  :bind (("C-c l l" . eglot)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)))

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

(use-package prescient
  :ensure t
  :custom (completion-styles '(basic partial-completion prescient))
  :config (prescient-persist-mode +1))

(use-package pyenv-mode
  :ensure t
  :config (pyenv-mode))

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

(if (treesit-ready-p 'rust)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

(if (treesit-ready-p 'typescript)
    (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode)))

(if (treesit-ready-p 'yaml)
    (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))

;; FUNCTIONS
(defun goedev/switch-to-previous-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(defun goedev/switch-to-default-buffer-group ()
  "Switch to default buffer group."
  (ibuffer-switch-to-saved-filter-groups "default"))

;; THEME
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))
(load-theme 'modus-vivendi :no-confirm)

;; KEYMAPS
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "C-c SPC") 'comment-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x O") 'goedev/switch-to-previous-window)
(global-set-key (kbd "C-z") 'undo-only)

;; HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'ibuffer-mode-hook 'goedev/switch-to-default-buffer-group)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
