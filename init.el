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
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode   . visual-line-mode))
  :bind (("<mouse-4>" . scroll-down-line)
         ("<mouse-5>" . scroll-up-line)
         ("C-c SPC"   . comment-line)
         ("C-c o"     . previous-window-any-frame)
         ("C-x C-b"   . ibuffer)
         ("C-z"       . undo-only)))

(use-package eglot
  :bind (("C-c l l" . eglot)
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)))

(use-package flymake
  :bind (("C-c l n" . flymake-goto-next-error)
         ("C-c l p" . flymake-goto-prev-error)))

;; AI TOOLS
(use-package chatgpt
  :ensure t)

(use-package codegpt
  :ensure t)

(use-package copilot
  :ensure t
  :bind ("C-c g i" . copilot-accept-completion))

(use-package openai
  :ensure t
  :custom (openai-key (getenv "OPENAI_KEY")))

;; FILE MODES
(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom (markdown-command "multimarkdown"))

;; EXTERNAL PACKAGES
(use-package consult
  :ensure t
  :bind (("C-x M-:" . consult-complex-command)
         ("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-y"     . consult-yank-pop)
         ("M-s d"   . consult-find)
         ("M-s r"   . consult-ripgrep))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom ((consult-find-args "find . -not ( -wholename */.* -prune -o -name node_modules -prune )")
           (completion-in-region-function 'consult-completion-in-region)))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(use-package embark
  :ensure t
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :custom ((completion-styles             '(orderless basic))
           (completion-category-overrides '((file (styles basic partial-completion))))))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package expand-region
  :ensure t
  :bind ("C-c w" . er/expand-region))

(use-package magit
  :ensure t
  :hook (after-save . magit-after-save-refresh-status))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m" . mc/edit-lines)
         ("C-c j" . mc/mark-next-like-this)
         ("C-c k" . mc/mark-previous-like-this)
         ("C-c n" . mc/mark-all-like-this)))

(use-package poetry
  :ensure t)

(use-package savehist
  :ensure t
  :init (savehist-mode))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config (global-treesit-auto-mode))

(use-package vertico
  :ensure t
  :init (vertico-mode))

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
