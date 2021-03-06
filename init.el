;; Packages
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(setq gc-cons-threshold 100000000)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action 'projectile-dired)
  (projectile-mode +1))

(use-package move-text
  :bind
  (("C-S-<up>" . move-text-up)
   ("C-S-<down>" . move-text-down))
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode)
		(ggtags-mode 1)))))

(use-package magit
  :ensure t)

;; IDO setup
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-enable-last-directory-history nil)
(setq ido-record-commands nil)
(setq ido-max-work-directory-list 0)
(setq ido-max-work-file-list 0)
(ido-mode +1)

;; Dired setup
(defun always-hide-info-in-dired ()
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'always-hide-info-in-dired)

;; Compile mode
(setq-default compilation-scroll-output t)

;; UI
(tool-bar-mode -1)
(setq default-cursor-type 'bar)
(setq inhibit-startup-message t)
(global-hl-line-mode t)
(global-display-line-numbers-mode)
(toggle-horizontal-scroll-bar 1)
(setq column-number-mode t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3))
(setq scroll-conservatively 10000)
(set-frame-font "DejaVu Sans Mono 12" nil t)
(load-theme 'tango)
(set-face-background 'hl-line "#E0E0E0")
(set-face-foreground 'highlight nil)
(setq frame-title-format "%f")
(setq-default isearch-allow-scroll 'unlimited)

(setq-default tab-width 8)
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#DECAFF")
(add-hook 'buffer-menu-mode-hook
	  (lambda () (setq show-trailing-whitespace nil)))
(setq-default speedbar-show-unknown-files t)
(setq speedbar-directory-unshown-regexp "^$")

;; Backups
(setq
   backup-by-copying t
   backup-directory-alist '(("." . "~/.backupemacs/"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
	(modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
	(let* ((to (car map))
	       (from (quail-get-translation
		      (cadr map) (char-to-string to) 1)))
	  (when (and (characterp from) (characterp to))
	    (dolist (mod modifiers)
	      (define-key local-function-key-map
		(vector (append mod (list from)))
		(vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

;; Hotkeys on russian layout
(reverse-input-method 'russian-computer)

(server-start)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold 100000000))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook 'my-minibuffer-exit-hook)

(setq gc-cons-threshold 800000)
