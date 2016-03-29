(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'auto-complete-config)
(ac-config-default)

;; Web Mode Settings
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cs'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-engines-alist
      '(("angular" . "\\.html\\'"))
      )
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)


;; Mode Settings
(global-git-gutter-mode t)
(indent-guide-global-mode)
(setq indent-guide-recursive t)

;; Defaults
(show-paren-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq inhibit-startup-message t)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
;;(Load-theme 'solarized t)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))

;; Keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
