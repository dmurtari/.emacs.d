(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) 

(require 'auto-complete-config)
(ac-config-default)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq inhibit-startup-message t)
(setq backup-inhibited t)
(setq auto-save-default nil)

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
