;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-one-light)

(setq doom-font (font-spec :family "monospace" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 17))

(after! frog-jump-buffer
  (map! :leader "," `frog-jump-buffer))
;;   default is "+ivy/switch-workspace-buffer"
(use-package! frog-jump-buffer :init)

(use-package! protobuf-mode :init)

(after! counsel
  (define-key!
    [remap evil-show-marks]          #'counsel-evil-marks
    )
  )

(setq-default evil-escape-key-sequence "kj") ;; from stack quection: https://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/")
(setq org-journal-dir "~/org/Journal/")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-date-format "%d.%m.%Y")

(after! org
  (setq org-capture-todo-file "gtd/inbox.org")
  ;; agenda files are roots to org agenda search
  (setq org-agenda-files '(;;"~/org/gtd/inbox.org"
                          "~/org/gtd/gtd.org"
                          "~/org/gtd/tickler.org"
                          "~/org/Work"
        "~/org/Journal");; temporarily add journal files, until I move completely to agenda and gtd setup
        )

  (setq org-todo-keywords `((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  ;; GTD: setup for [[https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html][gtd inspired]] refile and capture
  (setq org-refile-targets '(("~/org/gtd/gtd.org" :maxlevel . 3)
                            ("~/org/gtd/someday.org" :level . 1)
                            ("~/org/gtd/tickler.org" :maxlevel . 2)
                            ("~/org/writing-inbox.org" :maxlevel . 2)))

  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                (file+headline "~/org/gtd/inbox.org" "Tasks")
                                "* TODO %i%?")
                                ("T" "Tickler" entry
                                (file+headline "~/org/gtd/tickler.org" "Tickler")
                                "* %i%? \n %U")))
  (setq org-modules '(ol-bibtex org-habit))
)

(after! org-agenda
 (load-file "~/.doom.d/norang-ca-org-mode.el")
 (add-to-list 'org-agenda-custom-commands `,bh/org-agenda-view)
)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; 'visual - having numbers for lines created by virtual line breaks for long lines
;; corresponds with (setq evil-respect-visual-line-mode t) in init.el
(setq display-line-numbers-type 'visual)


(setq projectile-project-search-path '("~/Documents/"))
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(after! eshell

  ;; copied from https://github.com/hlissner/doom-emacs/blob/3f2c4b80e9adf1c7809e3fe3c299030cbcc7de33/modules/term/eshell/config.el
        (defun +eshell--current-git-branch ()
        ;; TODO Refactor me
        (cl-destructuring-bind (status . output)
        (doom-call-process "git" "symbolic-ref" "-q" "--short" "HEAD")
        (if (equal status 0)
                (format " [%s]" output)
        (cl-destructuring-bind (status . output)
                (doom-call-process "git" "describe" "--all" "--always" "HEAD")
                (if (equal status 0)
                (format " [%s]" output)
                "")))))

        (defun +eshell-my-prompt-fn ()
        "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
        (require 'shrink-path)
        (concat (if (bobp) "" "\n")
                (let ((pwd (eshell/pwd)))
                (propertize pwd
                                'face '+eshell-prompt-pwd))
                (propertize (+eshell--current-git-branch)
                        'face '+eshell-prompt-git-branch)
                "\n"
                (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
                " "))


        (setq eshell-prompt-function #'+eshell-my-prompt-fn)
        (setq eshell-prompt-regexp "^.* λ ")
  )
(after! em-term
  (pushnew! eshell-visual-commands "ssh" "sbt"))



(require `epa-file)
(epa-file-enable)
