;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; INIT
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024)))) ; 2mb

;; THEME
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active   unspecified)
        (border-mode-line-inactive unspecified)))
(load-theme 'modus-vivendi :no-confirm)

(when (display-graphic-p)
  (tool-bar-mode   0)
  (scroll-bar-mode 0)
  (set-face-attribute 'default     nil :family "JetBrains Mono" :height 150)
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 150))

;; INTERNAL PACKAGES
(use-package emacs
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode   . visual-line-mode)
         (prog-mode   . hs-minor-mode))
  :bind (("C-c SPC"   . comment-line)
         ("C-x O"     . previous-window-any-frame)
         ("C-x C-b"   . ibuffer)
         ("C-z"       . undo-only)))

(use-package eglot
  :ensure t
  :bind (("C-c l l" . eglot)
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)))

(use-package savehist
  :init (savehist-mode))

(use-package org
  :hook (org-mode . org-indent-mode))

;; EXTERNAL PACKAGES
(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("M-g f"   . consult-flymake)
         ("M-g g"   . consult-goto-line)
         ("M-s d"   . consult-find)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ("M-s s"   . consult-line)
         ("M-s S"   . consult-line-multi)
         ("M-s r"   . consult-ripgrep)
         ("M-y"     . consult-yank-pop))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom (consult-find-args "find . -not ( -wholename */.* -prune -o -name node_modules -prune )"))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(use-package orderless
  :ensure t
  :custom ((completion-styles             '(orderless basic))
           (completion-category-overrides '((file (styles basic partial-completion))))))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-icon nil))

(use-package expand-region
  :ensure t
  :bind ("C-c w" . er/expand-region))

(use-package magit
  :ensure t
  :hook (after-save . magit-after-save-refresh-status))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c j" . mc/mark-next-like-this)
         ("C-c k" . mc/mark-previous-like-this)
         ("C-c n" . mc/mark-all-like-this)))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config (global-treesit-auto-mode))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package corfu
  :ensure t
  :init (global-corfu-mode))

;; Emacs 31 draws corfu popups in the TTY itself.
(use-package corfu-terminal
  :ensure t
  :unless (or (display-graphic-p) (featurep 'tty-child-frames))
  :config (corfu-terminal-mode +1))

(use-package golden-ratio
  :ensure t
  :bind ("C-c o" . golden-ratio))

(use-package breadcrumb
  :ensure t
  :config (breadcrumb-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; init.el ends here
