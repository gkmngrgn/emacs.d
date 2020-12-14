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

;; Performance
(setq gc-cons-threshold 600000000)
(setq read-process-output-max (* 4096 1024))  ; 4mb

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
(defvar font-default "Iosevka")
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

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Global keymap settings
(global-set-key (kbd "C-c SPC") 'comment-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-p")     'backward-paragraph)
(global-set-key (kbd "M-n")     'forward-paragraph)

;; GUI settings
(defun hdpi? ()
  "Check if you're using alienware or thinkpad."
  (and (= 3840 (display-pixel-width))
       (= 2160 (display-pixel-height))))

(if (display-graphic-p)
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      (set-frame-font font-default)

      (if (hdpi?)
          ;; HiDPI settings
          (progn
            (set-face-attribute 'default nil :height 150)
            (fringe-mode 16))

        ;; Default DPI settings
        (set-face-attribute 'default nil :height 140))))

;; Package configurations
(use-package avy
  :bind (("M-g g" . avy-goto-char-2)
         ("M-g f" . avy-goto-char))
  :config
  (avy-setup-default))

(use-package ace-window
  :config
  (custom-set-faces
   '(aw-leading-char-face ((t (:height 1.0)))))
  :defer t
  :diminish
  :init
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-background nil)
  (setq aw-dispatch-always t))

(use-package cmake-mode)

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

(use-package company-solidity
  :after solidity-mode
  :config
  (push 'company-solidity company-backends)
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

(use-package editorconfig
  :diminish
  :config
  (setq editorconfig-exclude-modes
        '(common-lisp-mode
          emacs-lisp-mode
          lisp-mode
          web-mode))
  (editorconfig-mode 1))

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

(use-package format-all
  :defer t)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :diminish)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode t)
  :defer t)

(use-package lsp-ivy
  :after counsel
  :commands (lsp-ivy-workspace-symbol)
  :defer t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :diminish
  :hook ((c++-mode        . lsp-deferred)
         (cmake-mode      . lsp-deferred)
         (css-mode        . lsp-deferred)
         (go-mode         . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (python-mode     . lsp-deferred)
         (rust-mode       . lsp-deferred)
         (web-mode        . lsp-deferred)
         (yaml-mode       . lsp-deferred)
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

(use-package lsp-dart
  :hook (dart-mode . lsp-deferred))

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

(use-package modus-operandi-theme
  :config
  ;; (load-theme 'modus-operandi t)
  (set-face-attribute 'mode-line nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :inherit 'mode-line))

(use-package modus-vivendi-theme
  :config
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'mode-line nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :inherit 'mode-line))

(use-package origami
  :config
  (global-origami-mode t))

(use-package org
  :init
  (setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE")))
  (setq org-log-done t))

(use-package prescient
  :commands (prescient-persist-mode)
  :config
  (prescient-persist-mode t)
  :defer t)

(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook prog-mode)

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package slime-company)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company))
  :diminish)

(use-package solidity-mode
  :defer t)

(use-package solidity-flycheck
  :after solidity-mode
  :defer t)

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package undo-fu
  :bind (("C-z" . undo-fu-only-undo)
         ("C-M-z" . undo-fu-only-redo))
  :defer t)

(use-package unfill
  :bind (("C-M-q" . unfill-paragraph))
  :defer t)

(use-package which-key
  :config
  (which-key-mode)
  :diminish)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :diminish (yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package zoom
  :config
  (zoom-mode t)
  :diminish)

;; File modes
(use-package bazel-mode
  :defer t)

(use-package cargo
  :after rust-mode
  :defer t
  :diminish (cargo-minor-mode)
  :hook (rust-mode . cargo-minor-mode))

(use-package cython-mode)

(use-package dart-mode
  :defer t
  :hook (dart-mode . (lambda() (local-unset-key (kbd "C-c C-i")))))

(use-package dockerfile-mode
  :defer t)

(use-package gdscript-mode
  :defer t
  :hook (gdscript-mode-hook . setup-gdscript))

(use-package go-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package lisp-mode
  :defer t
  :diminish eldoc-mode
  :ensure nil) ;; pre-installed package

(use-package markdown-mode
  :config
  (set-face-attribute 'markdown-table-face nil :font font-default)
  (set-face-attribute 'markdown-pre-face nil :font font-default)
  :init
  (setq markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package po-mode
  :defer t)

(use-package powershell
  :defer t)

(use-package protobuf-mode
  :defer t)

(use-package rust-mode
  :defer t)

(use-package scss-mode
  :defer t
  :mode (("\\.scss$" . scss-mode)
         ("\\.sass$" . scss-mode)))

(use-package text-mode
  :diminish (visual-line-mode)
  :ensure nil  ;; pre-installed package
  :init
  (add-to-list 'auto-mode-alist '("\\`/tmp/neomutt-" . mail-mode)))

(use-package vue-mode
  :defer t
  :init
  (add-hook 'vue-mode-hook (lambda () (setq syntax-ppss-table nil)))
  (add-hook 'mmm-mode-hook
            (lambda ()
              (set-face-background 'mmm-default-submode-face nil))))

(use-package web-mode
  :defer t
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-engines-alist '(("django" . "\\.html$")))
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-block-padding 0)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package yaml-mode
  :defer t)

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
      ("k"   flycheck-previous-error             "Previous"))

    (defhydra hydra-focus (:columns 4)
      "Focus"
      ("+"   text-scale-increase                 "Zoom in")
      ("-"   text-scale-decrease                 "Zoom out")
      ("j"   diff-hl-next-hunk                   "Next diff")
      ("k"   diff-hl-previous-hunk               "Previous diff")

      ("f"   format-all-buffer                   "Format")
      ("s"   yas-insert-snippet                  "Insert snippet")
      ("o"   origami-toggle-node                 "Toggle node")
      ("t"   origami-toggle-all-nodes            "Toggle all nodes"))))

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
