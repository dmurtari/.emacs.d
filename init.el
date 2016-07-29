(setq exec-path (append '("/Users/domenic.murtari/.nvm/versions/node/v5.10.0/bin/") exec-path))

(setq my-packages '(ac-html
                    ac-html-angular
                    ac-html-bootstrap
                    ac-html-csswatcher
                    ac-js2
                    angular-mode
                    angular-snippets
                    auto-complete
                    coffee-mode
                    docker
                    dockerfile-mode
                    feature-mode
                    git-gutter
                    grizzl
                    highlight-symbol
                    indent-guide
                    inf-ruby
                    inf-mongo
                    jade-mode
                    js2-mode
                    json-mode
                    json-reformat
                    less-css-mode
                    magit
                    markdown-mode
                    multi-term
                    nodejs-repl
                    projectile
                    scss-mode
                    skewer-mode
                    tss
                    web-mode
                    ))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(mapc #'package-install my-packages)

;; Requires
(require 'ac-html)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'feature-mode)
(require 'highlight-symbol)
(require 'ido)
(require 'less-css-mode)
(require 'inf-mongo)
(require 'multi-term)
(require 'nodejs-repl)
(require 'tss)
(require 'typescript)
(require 'web-mode)
(require 'whitespace)

;; AutoComplete Mode Config
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode)
(add-to-list 'ac-modes 'web-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; Web Mode Config
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-engines-alist
      '(("angular" . "\\.html\\'")
        ("scss"    . "\\.scss\\'"))
      )
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)
(add-hook 'html-mode-hook 'setup-ac-for-html)
(add-to-list 'web-mode-ac-sources-alist
             '("html" . (ac-source-html-tag
                         ac-source-html-attr
                         ac-source-html-attrv)))
(add-hook 'web-mode 'ac-html-angular+)

;; Mode Settings
(global-whitespace-mode t)
(highlight-symbol-mode t)
(setq whitespace-style '(tabs trailing tab-mark spaces newline))
(global-git-gutter-mode t)
(indent-guide-global-mode)
(setq indent-guide-recursive t)
(projectile-global-mode)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(setq js2-highlight-level 3)
(setq js2-indent-on-enter-key t)
(setq typescript-indent-level 2)
(setq highlight-symbol-nav-mode t)
(setq tramp-default-method "ssh")
(setq system-uses-terminfo nil)

;; File Type Configurations
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.feature\\'" . feature-mode))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

;; Defaults
(setq-default cursor-type 'bar)
(tool-bar-mode -1)
(tss-config-default)
(show-paren-mode 1)
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(electric-pair-mode 1)
(electric-indent-mode 1)
(setq-default column-number-mode t)
(global-hl-line-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq inhibit-startup-message t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq global-subword-mode t)
(setq multi-term-program "/bin/zsh")
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(set-face-attribute 'default nil :height 100)
(setq-default line-spacing 3)
(load-theme 'solarized t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (solarized)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(frame-background-mode (quote dark)))

;; Macros
(fset 'move-line-up
   "\C-a\C-k\C-k\C-p\C-y\C-p")
(fset 'move-line-down
   "\C-a\C-k\C-k\C-n\C-y")
(fset 'kill-to-start-of-line
   "\C-[0\C-k")

;; Keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-i") 'imenu)
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; Functions
(defun setup-ac-for-html ()
  (require 'ac-html)
  (require 'ac-html-default-data-provider)
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-html-setup)
  (setq ac-sources '(ac-source-html-tag
                     ac-source-html-attr
                     ac-source-html-attrv))
  (auto-complete-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun auto-complete-mode-maybe ()
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

(defun node-repl () (interactive)
       (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive"))
       (node-repl))
