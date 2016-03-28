(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'auto-complete-config)
(ac-config-default)

;; Mode Settings
(global-whitespace-mode t)
(setq whitespace-action '(auto-cleanup))
(global-git-gutter-mode t)

;; Defaults
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq inhibit-startup-message t)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

;; Keybindings
(global-set-key (kbd "C-x g") 'magit-status)
