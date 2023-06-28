;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; (unpin! org-roam company-org-roam)
;; (package! org-roam
;;   :recipe (:host github :repo "org-roam/org-roam"))

(package! org-habit-plus
  :recipe (:host github :repo "myshevchuk/org-habit-plus"))

(package! frog-jump-buffer)

(package! protobuf-mode)

;; (package! evil-lispy)

(package! mermaid-mode)

(package! ob-mermaid)

(package! auto-overlays)

;; integration with 0x0.st (sharing service)
;; (package! 0x0
;;   :recipe (:host gitlab :repo "willvaughn/emacs-0x0"))

(package! ox-ssh)

(package! eshell-z :disable t)

(package! consult-dir)

(package! org-appear)

(package! org-pomodoro-third-time
  :recipe (:host github :repo "telotortium/org-pomodoro-third-time"))


(package! evil-owl)

(package! kubel)
(package! kubel-evil)

(package! keycast)

(package! nov)                          ; reading epub

;; (package! org-cv
;;   :recipe (:host gitlab :repo "Titan-C/org-cv"))

(package! format-all
  :recipe (:host github :repo "lassik/emacs-format-all-the-code"))

(package! ef-themes)

(package! chatgpt-shell
  :recipe (:host github :repo "xenodium/chatgpt-shell"))

;; https://codeberg.org/akib/emacs-eat.git
(package! eat
  :recipe (:host codeberg :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(package! codeium :recipe (:host github :repo "Exafunction/codeium.el"))

(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
