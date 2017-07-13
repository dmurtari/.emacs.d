(setq exec-path (append '("/Users/domenic.murtari/.nvm/versions/node/v7.6.0/bin/") exec-path))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(add-hook 'after-init-hook 'global-company-mode)

;; Packages to load
(require 'ng2-mode)
(require 'whitespace)
(require 'git-gutter-fringe)

;; Defaults
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-git-gutter-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'editorconfig-apply)
(delete-selection-mode t)
(electric-indent-mode 1)
(electric-pair-mode 1)
(global-hl-line-mode t)
(ido-mode t)
(scroll-bar-mode -1)
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq css-indent-offset 2)
(setq global-subword-mode t)
(setq ido-enable-flex-matching t)
(setq inhibit-startup-message t)
(setq js-indent-level 2)
(setq x-select-enable-clipboard t)
(setq-default column-number-mode t)
(setq-default cursor-type 'bar)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(show-paren-mode 1)
(tool-bar-mode -1)
(transient-mark-mode t)
(setq multi-term-program "/bin/zsh")
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

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
  (setq windmove-wrap-around t )
  )

;; Tide Mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(setq tide-format-options '(:indentSize 2 :tabSize 2))
;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Theming
(load-theme 'monokai t)
(set-face-attribute 'default nil :height 100)
(setq-default line-spacing 2)

;; Whitespace Mode
(progn
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark)))
  (setq whitespace-line-column 120)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-safe-themes
   (quote
    ("87233846530d0b2c50774c74c4aca06a1472504c63ccd4ab2b1021b3e56a69e9" default)))
 '(package-selected-packages
   (quote
    (git-gutter-fringe darkokai-theme monokai-theme ng2-mode typescript-mode editorconfig monokai-alt-theme web-mode vue-mode tide projectile magit company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
