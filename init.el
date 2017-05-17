;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (abyss)))
 '(custom-safe-themes
   (quote
    ("f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "3b937e3107fa20627795a3a222765b6ef39472ef8c56d65003d413c6c6667cd0" default)))
 '(global-visual-line-mode nil)
 '(horizontal-scroll-bar-mode t)
 '(indicate-empty-lines t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (abyss-theme sass-mode highlight-parentheses emmet-mode list-packages-ext skewer-mode ztree js2-mode php-mode yaml-tomato yaml-mode web-mode tabbar scss-mode robe rinari rainbow-mode rails-new rails-log-mode multi-term llvm-mode jdee javarun javap javaimp javadoc-lookup java-snippets java-imports java-file-create jasmin jar-manifest-mode helm-rails haml-mode flycheck-haskell eruby-mode color-theme-monokai cmake-project cmake-mode cmake-ide clojurescript-mode clojure-mode auto-complete)))
 '(show-paren-mode t)
 '(speedbar-show-unknown-files t)
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; UI settings

;; remove emacs welcome screen
(setq inhibit-startup-message t)

;; load theme
(load-theme 'me t)

;; set bar cursor
(setq-default cursor-type 'bar)

;; line nunmbers
(global-linum-mode t)

;;frame-size
(add-to-list 'default-frame-alist '(height . 200))
(add-to-list 'default-frame-alist '(width . 103))

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)


(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)


;80 column
(global-column-enforce-mode t)
(add-hook 'js2-mode-hook 'global-column-enforce-mode)
(add-hook 'css-mode-hook 'global-column-enforce-mode)
(add-hook 'html-mode-hook 'global-column-enforce-mode)

;speed-bar
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'sr-speedbar)


(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

(defun select-next-window ()
  (other-window 1))

(defun my-sr-speedbar-open-hook ()
  (add-hook 'speedbar-before-visiting-file-hook 'select-next-window t)
  (add-hook 'speedbar-before-visiting-tag-hook 'select-next-window t)
  )

(advice-add 'sr-speedbar-open :after #'my-sr-speedbar-open-hook)


(setq hl-paren-colors
      '(;"#8f8f8f" ; this comes from Zenburn
                   ; and I guess I'll try to make the far-outer parens look like this
        "orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
(global-auto-complete-mode t)


;;sas-mode
(require 'sass-mode)


;;preserve layout under lisp folder
(require 'layout-restore)
(global-set-key [?\C-c ?l] 'layout-save-current)
(global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)
(global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)


;;Clean Buffer List
(setq clean-buffer-list-delay-general (* 12 60))
(setq clean-buffer-list-delay-special (* 12 60))

