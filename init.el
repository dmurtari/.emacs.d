(setq exec-path (append '("/Users/domenic.murtari/.nvm/versions/node/v7.6.0/bin/") exec-path))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Packages to load
(require 'ng2-mode)
(require 'whitespace)
(require 'vue-mode)
(require 'git-gutter-fringe)
(require 'multiple-cursors)
(require 'color-theme-sanityinc-tomorrow)

;; Save autosave files in temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Hooks
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-git-gutter-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'editorconfig-apply)
(add-hook 'after-init-hook 'highlight-numbers-mode)

;; Defaults
(delete-selection-mode t)
(electric-indent-mode 1)
(electric-pair-mode 1)
(global-hl-line-mode t)
(global-whitespace-mode t)
(ido-mode t)
(desktop-save-mode)
(scroll-bar-mode -1)
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq css-indent-offset 2)
(setq global-subword-mode t)
(setq ido-enable-flex-matching t)
(setq inhibit-startup-message t)
(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq x-select-enable-clipboard t)
(setq-default column-number-mode t)
(setq-default cursor-type 'bar)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(show-paren-mode 1)
(tool-bar-mode -1)
(transient-mark-mode t)
(setq multi-term-program "/bin/zsh")

;; Keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-i") 'imenu)
(progn
  ;; Change selected window pane with shift + arrow
  (require 'windmove)
  (windmove-default-keybindings)
  (setq windmove-wrap-around t ))
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Theming
(load-theme 'monokai t)
(load-theme 'ample t)
(load-theme 'ample-flat t)
(load-theme 'zenburn t)
(load-theme 'sanityinc-tomorrow-night t)
(enable-theme 'sanityinc-tomorrow-night)

(set-face-attribute 'default nil :height 100)
(setq-default line-spacing 2)
(setq mmm-submode-decoration-level 0)

;; Custom functions
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))


;; MODE CUSTOMIZATIONS

;; Tide Mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq company-tooltip-align-annotations t)
  (setq tide-format-options '(:indentSize 2 :tabSize 2))  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(add-hook 'typescript-mode-hook 'setup-tide-mode)

;; Whitespace Mode
(setq whitespace-style (quote (face tabs newline space-mark tab-mark)))
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
(setq whitespace-line-column 120)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (highlight-numbers color-theme-sanityinc-tomorrow ample-theme zenburn-theme multiple-cursors git-gutter-fringe darkokai-theme monokai-theme ng2-mode typescript-mode editorconfig monokai-alt-theme web-mode vue-mode tide projectile magit company))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
