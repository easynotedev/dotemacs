(deftheme custom
  "Created 2017-07-12.")

(custom-theme-set-variables
 'custom
 '(ansi-color-names-vector ["#32302F" "#FB4934" "#B8BB26" "#FABD2F" "#83A598" "#D3869B" "#17CCD5" "#EBDBB2"])
 '(custom-safe-themes (quote ("1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" "a1cd268f214d0ee89224769eff3bfcc874446999adbe830d3fbce41c6564e36e" "c9321e2db48a21fc656a907e97ee85d8cd86967855bf0bed3998bcf9195c758b" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "f206888744ed84e592521591efedd6954625d6c2aac5d45d81c2ea16d1cd4a33" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (paganini-theme emmet-mode handlebars-sgml-mode pug-mode helm-ag projectile xref-js2 company-tern flycheck js2-refactor js2-mode web-mode sass-mode auto-complete)))
 '(safe-local-variable-values nil)
 '(speedbar-default-position (quote right))
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 8) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (left-fringe . 0))))
 '(speedbar-show-unknown-files t)
 '(speedbar-verbosity-level 0)
 '(horizontal-scroll-bar-mode t)
 '(menu-bar-mode t)
 '(tool-bar-mode nil))

(custom-theme-set-faces
 'custom
 '(column-enforce-face ((t (:background "dim gray" :foreground "white smoke")))))

(provide-theme 'custom)
