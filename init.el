;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#32302F" "#FB4934" "#B8BB26" "#FABD2F" "#83A598" "#D3869B" "#17CCD5" "#EBDBB2"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(custom-safe-themes
   (quote
    ("c9321e2db48a21fc656a907e97ee85d8cd86967855bf0bed3998bcf9195c758b" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "f206888744ed84e592521591efedd6954625d6c2aac5d45d81c2ea16d1cd4a33" default)))
 '(horizontal-scroll-bar-mode t)
 '(icicle-command-abbrev-alist (quote ((describe-variable k 2))))
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(package-selected-packages
   (quote
    (projectile xref-js2 company-tern flycheck js2-refactor js2-mode web-mode sass-mode auto-complete)))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(safe-local-variable-values nil)
 '(speedbar-default-position (quote right))
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 8)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-show-unknown-files t)
 '(speedbar-verbosity-level 0)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; load path to /lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ///////////////
;; UI settings ///
;; ///////////////

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; set bar cursor
(setq-default cursor-type 'bar)

;; remove emacs welcome screen
(setq inhibit-startup-message t)

;; line nunmbers
(global-linum-mode t)

;;frame-size
(add-to-list 'default-frame-alist '(height . 200))
(add-to-list 'default-frame-alist '(width . 103))

;;; Backup files
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacsbackupfs"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; reload init.el
(global-set-key [f5] '(lambda() (interactive) (load-file "~/.emacs.d/init.el")))

;; toggle menu-bar
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)
;; toggle tool-bar
(global-set-key [f8] 'toggle-tool-bar-mode-from-frame)

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)


(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;;80 column
(require 'column-enforce-mode)
(global-column-enforce-mode t)
(add-hook 'js2-mode-hook 'global-column-enforce-mode)
(add-hook 'css-mode-hook 'global-column-enforce-mode)
(add-hook 'html-mode-hook 'global-column-enforce-mode)

;; highlight-parentheses)
(require 'highlight-parentheses)
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

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; ///////////
;; Javascript/
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; js2-refactor is a JavaScript refactoring library for emacs.Refactorings go
;; from inlining/extracting variables to converting ternary operators to if statements. The
;; https://github.com/magnars/js2-refactor.el
;; provides the full list of keybindings.
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(require 'company)
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
                           
;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;; /////////////////
;; Added Functions//
;; /////////////////

(desktop-save-mode 1)

(require 'multi-term)
(require 'projectile)
(projectile-mode 1)

;speed-bar
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

(defun select-next-window ()
  (other-window 1))

(defun my-sr-speedbar-open-hook ()
  (add-hook 'speedbar-before-visiting-file-hook 'select-next-window t)
  (add-hook 'speedbar-before-visiting-tag-hook 'select-next-window t)
  )

(advice-add 'sr-speedbar-open :after #'my-sr-speedbar-open-hook)

;; enhances minibuffer completion
(icy-mode 1)

;; removes menu-bar & tool-bar real state 
(menu-bar-mode -1)
(tool-bar-mode -1)

