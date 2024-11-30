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
  (load-theme 'modus-vivendi :no-confirm)
  :custom (modus-themes-mode-line '(borderless))
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode   . visual-line-mode))
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
  :ensure t
  :init (savehist-mode))

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

(use-package copilot-chat
  :ensure t)

(use-package simpleclip
  :ensure t
  :config (simpleclip-mode))

(use-package golden-ratio
  :ensure t
  :bind ("C-c o" . golden-ratio))

(use-package sideline
  :ensure t)

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-flymake-display-mode 'point
              sideline-backends-right       '(sideline-flymake)))

(use-package sideline-blame
  :ensure t
  :init
  (setq sideline-backends-left '((sideline-blame . down))))

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
