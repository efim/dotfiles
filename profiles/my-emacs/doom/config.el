;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")
(setq default-input-method "russian-computer")
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
;; (setq
;;  doom-font (font-spec :family "monospace" :size 16 :weight 'semi-light)
;;  doom-variable-pitch-font (font-spec :family "sans" :size 17))
;;
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-monokai-machine)

(setq
 doom-font (font-spec :name "Iosevka" :size 20 )
 doom-big-font (font-spec :name "Iosevka" :size 27 :weight `light)
 doom-font-increment 1
 ;; doom-variable-pitch-font (font-spec :family "Playfair Display" :size 24)
 doom-variable-pitch-font (font-spec :family "Liberation Serif" :size 24)
 )

(after! frog-jump-buffer
  (map! :leader "," `frog-jump-buffer)
  (setq frog-jump-buffer-default-filter 'frog-jump-buffer-filter-same-project))
;;   default is "+ivy/switch-workspace-buffer"

(use-package! frog-jump-buffer :init)

(use-package! protobuf-mode :init)

(use-package! mermaid-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode)))

(use-package! ob-mermaid
  :init
  (setq ob-mermaid-cli-path "/home/efim/.nix-profile/bin/mmdc"))

(use-package! lispyville
  :init
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement t)
          additional-movement
          slurp/barf-lispy
          additional
          additional-insert
          wrap)))
;; (use-package! evil-lispy :init)
;; ;; https://github.com/sp3ctum/evil-lispy
;; ;; make evil-lispy start in the modes you want
;; (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)

(after! counsel
  (define-key!
    [remap evil-show-marks]          #'counsel-evil-marks
    )
  )

(setq-default evil-escape-key-sequence "kj") ;; from stack quection: https://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; ;; roam thingis
;; (setq org-roam-v2-ack t)
;; (setq org-roam-directory "~/org/")
;; (use-package! org-roam
;;   :after org
;;   :commands
;;   (org-roam-buffer
;;    org-roam-setup
;;    org-roam-capture
;;    org-roam-node-find)
;;   :config
;;   (org-roam-setup))
;; (add-to-list 'load-path ".")
;; (require `testing-roam-config-w-hydra)
(load-file "~/.doom.d/testing-roam-config-w-hydra.el")
(setq +org-roam-open-buffer-on-find-file nil)

;; (map! (:leader (:prefix "n"
;;         (:prefix-map ("r" . "roam")
;;          :desc "Org Roam Capture"              "c" #'org-roam-capture
;;          :desc "Find file"                     "f" #'org-roam-node-find
;;          :desc "Insert"                        "i" #'org-roam-node-insert
;;          :desc "Org Roam"                      "r" #'org-roam-buffer-toggle
;;          :desc "Tag"                           "t" #'org-roam-tag-add
;;          :desc "Un-tag"                        "T" #'org-roam-tag-delete
;;          ))))


(setq org-directory "~/org/")
(setq org-journal-dir "~/org/Journal/")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-date-format "%d.%m.%Y")

;; Show hidden emphasis markers
(use-package! org-appear
  :hook (org-mode . org-appear-mode))

(after! org
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook
          (lambda ()
            (evil-local-set-key 'motion "gk" #'org-backward-element)
            (evil-local-set-key 'motion "gj" #'org-forward-element)))


  (setq org-capture-todo-file "gtd/inbox.org")
  ;; agenda files are roots to org agenda search
  (setq org-agenda-files '(;;"~/org/gtd/inbox.org"
                           "~/org/gtd/gtd.org"
                           "~/org/gtd/tickler.org"
                           "~/org/Work/gtd/dins-gtd.org"
                           "~/org/Work/gtd/dins-tickler.org"
                           "~/org/Work/dino_systems.org" ;; retire in favor of gtd
                           )
        )

  (setq org-todo-keywords `((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  ;; GTD: setup for [[https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html][gtd inspired]] refile and capture
  (setq org-refile-targets '(("~/org/gtd/gtd.org" :maxlevel . 3)
                            ("~/org/gtd/someday.org" :level . 1)
                            ("~/org/gtd/tickler.org" :maxlevel . 2)
                            ("~/org/Work/gtd/dins-gtd.org" :maxlevel . 3)
                            ("~/org/Work/gtd/dins-someday.org" :level . 1)
                            ("~/org/Work/gtd/dins-tickler.org" :maxlevel . 2)
                            ("~/org/writing-inbox.org" :maxlevel . 2)))

  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/org/gtd/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("w" "Dins Todo [inbox]" entry
                                 (file+headline "~/org/Work/gtd/dins-inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/org/gtd/tickler.org" "Tickler")
                                 "* %i%? \n %U")))
  (add-to-list `org-modules `ol-bibtex)
  (add-to-list `org-modules `org-habit)
  (add-to-list `org-modules `org-habit-plus ))


(after! org-agenda
 (load-file "~/.doom.d/norang-ca-org-mode.el")
 (add-to-list 'org-agenda-custom-commands `,bh/org-agenda-view))

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
(use-package! vterm
  :init
  (defun vterm-quit-from-emacs-client ()
    (interactive)
    (vterm-send-C-d)
    (evil-quit))
  :bind
  (:map vterm-mode-map
   ("C-c C-q" . vterm-quit-from-emacs-client)))

(use-package! eshell
  :init
  (defun eshell-quit-from-emacs-client ()
    (interactive)
    (evil-quit))
  :bind
  (:map eshell-mode-map
   ("C-c C-q" . eshell-quit-from-emacs-client)))
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

;; turning on abbrev for automatic tempo + skeleton expansion
(setq-default abbrev-mode t)

(use-package! skempo
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          org-mode
          org-journal-mode
          nix-mode) . skempo-mode)
  :custom
  (skempo-completing-read t)
  (skempo-delete-duplicate-marks t)
  (skempo-update-identical-tags t)
  (skempo-skeleton-marks-support t)
  (skempo-always-create-tag t)
  (skempo-always-create-abbrev t)
  (skempo-mode-lighter " Sk")
  :bind
  (:map skempo-mode-map
   ("s-`" . skempo-complete-tag-or-call-on-region)
   ("M-g M-j" . skempo-forward-mark)
   ("M-g M-k" . skempo-backward-mark)))

(after! skempo
   (load-file "~/.doom.d/xFA25E-skempo-templates.el"))

(use-package! notmuch
  :config
  (evil-set-initial-state 'notmuch-search-mode 'normal)
  (evil-set-initial-state 'notmuch-hello-mode 'emacs)
  (defun my-maximized-notmuch ()
    ""
    (progn
      (notmuch)
      (doom/window-maximize-buffer)))
  (setq +notmuch-home-function #'my-maximized-notmuch)
  (define-key notmuch-hello-mode-map (kbd "q") #'+notmuch/quit)
  (setq +notmuch-sync-backend "systemctl --user start muchsync-server.service"))

(defun efim-config/manual-notmuch-email-update ()
  (interactive)
  "Trigger manual execution of muchsync."
  (start-process "manual muchsync" "*Messages*" "systemctl" "--user" "start" "muchsync-server.service"))

(after! embark (eval-when-compile
                 (defmacro my/embark-ace-action (fn)
                   `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
                      (interactive)
                      (with-demoted-errors "%s"
                        (require 'ace-window)
                        (let ((aw-dispatch-always t))
                          (aw-switch-to-window (aw-select nil))
                          (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump)))

(require `notifications)

(require `epa-file)

(define-key minibuffer-mode-map (kbd "C-u") #'universal-argument)
;; minibuffer-local-completion-map C-u
;; minibuffer-local-isearch-map C-u
;; minibuffer-local-ns-map C-u
;; minibuffer-local-must-match-map C-u

(use-package! 0x0)
(after! embark
  (define-key embark-region-map (kbd "U") #'0x0-dwim)) ; probably need to wrap with `after!

(use-package! ox-ssh)

(use-package! consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ;; :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; taken from karthinkgs https://karthinks.com/software/jumping-directories-in-eshell/
(defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
    (let ((eshell-dirs (delete-dups
                        (mapcar 'abbreviate-file-name
                                (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                             :narrow ?e
                                             :category file
                                             :face consult-file
                                             :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell
                                          consult-dir-sources)))
          (eshell/cd (substring-no-properties
                      (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                            (completing-read "cd: " eshell-dirs)))))))


;; adding agenda items into "appt" for notifications & enabling system notifications
(run-with-timer 2 1800 (lambda() (org-agenda-to-appt t)))
(load-file "~/.doom.d/app-sys-notify.el")
(advice-add #'appt-display-message :after #'my/appt-notifications-notify)



(use-package ensime-mode
  :ensure nil
  :load-path "~/Downloads/ensime-tng-3.0.0/lisp/"
  :commands ensime-mode
  :bind
  (:map ensime-mode-map
        ("M-." . ensime-jump-to-definition)
        ("C-c C-i t" . ensime-type-at-point)
        ("C-c C-i s" . ensime-symbol-at-point)
        ("C-c C-r i" . ensime-import-symbol-at-point)))

(set-company-backend! 'scala-mode
  'ensime-company)
(add-hook 'scala-mode-hook #'ensime-mode)

(use-package! gotham-theme :init)

(use-package! org-pomodoro-third-time :init)

(server-start)
(epa-file-enable)
