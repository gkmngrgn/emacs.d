;;; ~/.emacs.d/init.el --- GOEDEV personal emacs configuration file.

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

;; Packages
(use-package avy
  :bind (("M-g g" . avy-goto-char-2)
         ("M-g f" . avy-goto-line))
  :config
  (avy-setup-default))

(use-package ace-window
  :defer t
  :diminish
  :init
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-dispatch-always t))

(use-package company
  :bind ("C-c TAB" . company-complete-common)  ;; C-i and TAB are the same characters!
  :config
  (push 'company-capf company-backends)
  :diminish
  :hook ((after-init . global-company-mode))
  :init
  (setq-default company-dabbrev-ignore-case t
                company-dabbrev-code-ignore-case t)
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-require-match 'never
        company-show-numbers nil
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-tooltip-limit 10))

(use-package company-posframe
  :config
  (company-posframe-mode)
  :defer t
  :diminish
  :hook (company-mode . company-posframe-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode t)
  :defer t)

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
	 ("C-r"     . counsel-rg)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-d" . counsel-git)))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (if (not (display-graphic-p))
      (diff-hl-margin-mode)))

(use-package diminish)

(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind (("C-M-w" . er/expand-region))
  :defer t)

(use-package flycheck
  :diminish
  :init
  (global-flycheck-mode))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :diminish)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode t)
  :defer t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :diminish
  :hook ((go-mode         . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (python-mode     . lsp-deferred)
         (rust-mode       . lsp-deferred)
         (lsp-mode        . lsp-enable-which-key-integration))
  :init
  (setq-default lsp-completion-provider :capf)
  (setq-default lsp-rust-server 'rust-analyzer)
  (setq-default lsp-prefer-flymake nil)  ; flycheck is better
  (setq-default lsp-modeline-code-actions-segments '(name))

  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-idle-delay 0.500)
  (setq lsp-enable-snippet nil)  ; company is bettera
  (setq lsp-signature-doc-lines 10)
  (setq lsp-signature-auto-activate nil))

(use-package lsp-ivy
  :after counsel
  :commands (lsp-ivy-workspace-symbol)
  :defer t)

(use-package lsp-origami
  :after origami-mode
  :hook (lsp-after-open . lsp-origami-try-enable))

(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list)
  :defer t)

(use-package lsp-ui
  :defer t
  :commands (lsp-ui-mode)
  :init
  (setq lsp-ui-doc-enable nil))

(use-package magit
  :after diff-hl
  :config
  (add-hook 'magit-pre-refresh-hook  'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package modus-themes
  :ensure
  :init
  (setq modus-themes-slanted-constructs t
	modus-themes-bold-constructs nil)

  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package undo-fu
  :bind (("C-z" . undo-fu-only-undo)
         ("C-M-z" . undo-fu-only-redo))
  :defer t)

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
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'"       . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rust-mode
  :defer t)

(use-package yaml-mode
  :defer t)

;; Hydra settings
(use-package hydra
  :bind (("C-c e" . hydra-errors/body)
         ("C-c f" . hydra-focus/body))
  :config
  (defhydra hydra-errors (:pre (flycheck-list-errors)
                               :post (quit-windows-on "*Flycheck errors*")
                               :hint nil)
    "Errors"
    ("f"   flycheck-error-list-set-filter      "Filter")
    ("j"   flycheck-next-error                 "Next")
    ("k"   flycheck-previous-error             "Previous"))

  (defhydra hydra-focus (:columns 4)
    "Focus"
    ("j"   diff-hl-next-hunk                   "Next diff")
    ("k"   diff-hl-previous-hunk               "Previous diff")))

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
