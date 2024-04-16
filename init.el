;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; INTERNAL PACKAGES
(use-package emacs
  :init
  ;; GUI FONTS
  (if (display-graphic-p)
      (progn
        (defvar my-font "Iosevka Term")
        (defvar my-font-size 150)
        (set-frame-font my-font)
        (set-frame-font my-font)
        (set-face-attribute 'default nil :height my-font-size :font my-font)
        (set-face-attribute 'fixed-pitch nil :height my-font-size :font my-font)
        (scroll-bar-mode 0)
        (fringe-mode 0)
        (tool-bar-mode 0)))
  ;; THEME
  (load-theme 'modus-vivendi :no-confirm)
  :custom (modus-themes-mode-line '(borderless))
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode   . visual-line-mode))
  :bind (("C-c SPC"   . comment-line)
         ("C-x O"     . previous-window-any-frame)
         ("C-x C-b"   . ibuffer)
         ("C-z"       . undo-only))
  :mode (("\\.tsx?$"  . typescript-ts-mode)
         ("\\.ya?ml$" . yaml-ts-mode)))

(use-package eglot
  :bind (("C-c l l" . eglot)
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)))

(use-package savehist
  :init (savehist-mode))

;; FILE MODES
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-command "multimarkdown"))

;; EXTERNAL PACKAGES
(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("M-g f"   . consult-flymake)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s d"   . consult-find)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s r"   . consult-ripgrep)
         ("M-y"     . consult-yank-pop))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom ((consult-find-args "find . -not ( -wholename */.* -prune -o -name node_modules -prune )")
           (completion-in-region-function 'consult-completion-in-region)))

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

(use-package centered-cursor-mode
  :ensure t
  :demand
  :config
  (global-centered-cursor-mode))

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

(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>"   . 'copilot-accept-completion)
              ("TAB"     . 'copilot-accept-completion)
              ("C-TAB"   . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
