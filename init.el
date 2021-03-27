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
(delete-selection-mode 1)

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

;; Hydra settings
(use-package hydra
  :bind (("C-c e" . hydra-errors/body)
         ("C-c f" . hydra-focus/body))
  :config
  (defhydra hydra-focus (:columns 4)
    "Focus"
    ("+"   text-scale-increase                 "Zoom in")
    ("-"   text-scale-decrease                 "Zoom out")
    ("j"   diff-hl-next-hunk                   "Next diff")
    ("k"   diff-hl-previous-hunk               "Previous diff")))


;;; init.el ends here

;; Local Variables:
;; coding: utf-8
