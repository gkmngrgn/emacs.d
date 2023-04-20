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
(package-initialize)

(use-package expand-region
  :ensure t
  :bind ("C-c w" . er/expand-region))

(use-package magit
  :ensure t
  :config
  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)))

(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package prescient
  :ensure t
  :custom (completion-styles '(basic partial-completion prescient)))

(use-package treesit-auto
  :ensure t
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

;; (goedev/install-packages '(ctrlf
;;                            diff-hl
;;                            find-file-in-project
;;                            multiple-cursors
;;                            poetry
;;                            puni
;;                            pyenv-mode
;;                            unfill
;;                            vertico
;;                            vertico-prescient))

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

(defun goedev/configure-gui ()
  "Configure gui when you need to run Emacs with GUI."

  (defvar my-font "Jetbrains Mono")
  (defvar my-font-size 150)

  (set-frame-font my-font)

  (set-face-attribute 'default nil :height my-font-size :font my-font)
  (set-face-attribute 'fixed-pitch nil :height my-font-size :font my-font)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)

  (menu-bar-mode 0)
  (fringe-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0))

;; THEME
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))
(load-theme 'modus-vivendi :no-confirm)

(if (display-graphic-p)
    (goedev/configure-gui))

;; KEYMAPS
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "C-c SPC")   'comment-line)                      ;; comment/uncomment line.
(global-set-key (kbd "C-x C-b")   'ibuffer)                           ;; default buffer replacement.
;; (global-set-key (kbd "C-x C-d") 'ffip)
;; (global-set-key (kbd "C-c m")   'mc/edit-lines)
;; (global-set-key (kbd "C-c j")   'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c k")   'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c n")   'mc/mark-all-like-this)
;; (global-set-key (kbd "C-c q")   'unfill-paragraph)
(global-set-key (kbd "C-x O")   'goedev/switch-to-previous-window)
(global-set-key (kbd "C-z")     'undo-only)
(global-set-key (kbd "C-c l l") 'eglot)
(global-set-key (kbd "C-c l r") 'eglot-rename)
(global-set-key (kbd "C-c l f") 'eglot-format)
(global-set-key (kbd "C-c l n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c l p") 'flymake-goto-prev-error)

;; HOOKS
;; (add-hook 'before-save-hook          'delete-trailing-whitespace)
;; (add-hook 'magit-pre-refresh-hook    'diff-hl-magit-pre-refresh)
;; (add-hook 'magit-post-refresh-hook   'diff-hl-magit-post-refresh)
;; (add-hook 'term-mode-hook            'puni-disable-puni-mode)
;; (add-hook 'text-mode-hook            'visual-line-mode)
;; (add-hook 'ibuffer-mode-hook         'goedev/switch-to-default-buffer-group)

;; EXTENSIONS
;; (ctrlf-mode +1)
;; (puni-global-mode)
;; (vertico-mode)

;; (vertico-prescient-mode +1)
;; (prescient-persist-mode +1)

;; (global-diff-hl-mode)
;; (diff-hl-margin-mode)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
