;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2021 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

(menu-bar-mode 0)
(global-hl-line-mode)
(global-auto-revert-mode)
(delete-selection-mode 1)
(temp-buffer-resize-mode t)

;; UTF-8
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment   'utf-8)

;; Global Keymap Settings
(global-set-key (kbd "C-c SPC") 'comment-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-p")     'backward-paragraph)
(global-set-key (kbd "M-n")     'forward-paragraph)
(global-set-key (kbd "M-]")     'other-window)
(global-set-key (kbd "M-[")     'prev-window)

(defun prev-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(defalias 'yes-or-no-p 'y-or-n-p)

;; General Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook   'visual-line-mode)

;; Package Manager
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; GUI Settings
(if (display-graphic-p)
    (require 'init-gui))

;; Packages
(use-package diminish)

(use-package avy
  :bind
  (("M-g g" . 'avy-goto-char-2)
   ("M-g f" . 'avy-goto-line))
  :init
  (avy-setup-default))

(use-package company
  :bind
  ("C-c TAB" . 'company-complete-common) ;; C-i and TAB are the same characters!)
  :config
  (push 'company-capf company-backends)
  :diminish
  :init
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 2)
  (setq company-require-match 'never)
  (setq company-show-numbers nil)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above nil)
  (setq company-tooltip-limit 10)

  (global-company-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode t)
  :defer t)

(use-package counsel
  :bind
  (("M-x"     . 'counsel-M-x)
   ("C-r"     . 'counsel-rg)
   ("C-x C-f" . 'counsel-find-file)
   ("C-x C-d" . 'counsel-git)))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind
  ("C-M-w" . 'er/expand-region)
  :defer t)

(use-package flycheck
  :diminish
  :init
  (global-flycheck-mode))

(use-package flycheck-rust
  :after flycheck
  :hook
  (flycheck-mode-hook . 'flycheck-rust-setup))

(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  :diminish)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode t)
  :defer t)

(use-package lsp-dart
  :after dart-mode lsp-mode
  :defer t
  :init
  (add-hook 'dart-mode-hook #'lsp-deferred))

(use-package lsp-mode
  :diminish
  :init
  (setq lsp-completion-provider :capf)
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-prefer-flymake nil)  ; flycheck is better
  (setq lsp-modeline-code-actions-segments '(name))
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-idle-delay 0.500)
  (setq lsp-enable-snippet nil)  ; company is better
  (setq lsp-log-io nil)  ; if set to true can cause a performance hit
  (setq lsp-signature-doc-lines 10)
  (setq lsp-signature-auto-activate nil)

  (add-hook 'c-mode-hook          #'lsp-deferred)
  (add-hook 'go-mode-hook         #'lsp-deferred)
  (add-hook 'javascript-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook       #'lsp-deferred)
  (add-hook 'yaml-mode-hook       #'lsp-deferred)
  (add-hook 'lsp-mode-hook        #'lsp-enable-which-key-integration))

(use-package lsp-ivy
  :after lsp-mode ivy counsel
  :commands lsp-ivy-workspace-symbol
  :defer t)

(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]  #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c u")                 #'lsp-ui-imenu)
  :defer t
  :init
  (setq lsp-ui-doc-enable nil))

(use-package magit
  :after diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook  'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package modus-themes
  :bind
  ("<f5>" . 'modus-themes-toggle)
  :init
  (setq modus-themes-slanted-constructs t)
  (setq modus-themes-bold-constructs nil)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-local-pair 'web-mode "{" "}" :actions nil))

(use-package swiper
  :bind
  ("C-s" . 'swiper))

(use-package undo-fu
  :bind
  (("C-z"   . 'undo-fu-only-undo)
   ("C-M-z" . 'undo-fu-only-redo))
  :defer t)

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2))

(use-package which-key
  :config
  (which-key-mode)
  :diminish)

(use-package zoom
  :config
  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  :diminish)

;; File Modes
(use-package csv-mode
  :defer t)

(use-package dart-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package go-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package lisp-mode
  :defer t
  :diminish eldoc-mode
  :ensure nil
  :straight nil) ;; pre-installed package

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  ("README\\.md\\'" . 'gfm-mode)
  ("\\.md\\'"       . 'markdown-mode)
  ("\\.markdown\\'" . 'markdown-mode)
  :init
  (setq markdown-command "multimarkdown")
  (custom-set-faces
   '(markdown-code-face ((t (:inherit default))))))

(use-package powershell
  :defer t)

(use-package rust-mode
  :defer t)

(use-package vue-mode
  :defer t
  :init
  (add-hook 'vue-mode-hook (lambda () (setq syntax-ppss-table nil)))
  :config
  (setq mmm-submode-decoration-level 0))

(use-package yaml-mode
  :defer t)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
