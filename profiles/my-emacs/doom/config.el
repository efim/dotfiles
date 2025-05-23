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
(setq doom-theme 'ef-night
      ef-themes-to-toggle '(ef-night ef-light))

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
  (setq ob-mermaid-cli-path "~/.nix-profile/bin/mmdc"))

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
(setq-default evil-escape-unordered-key-sequence t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(load-file "~/.doom.d/testing-roam-w-transient.el")
(setq +org-roam-open-buffer-on-find-file nil)

(setq org-directory "~/org/")

(use-package! org-journal
  :config
  (setq org-journal-dir "~/org/Journal/"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%d.%m.%Y"))

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

  (setq my/journal-dir "~/org/Work/Journal/")

  (defun get-journal-file-today ()
    "Return filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y-%m-%d.org")))
      (expand-file-name (concat my/journal-dir daily-name))))

  (defun journal-file-today ()
    "Create and load a journal file based on today's date."
    (interactive)
    (find-file (get-journal-file-today)))

  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/org/gtd/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("w" "Dins Todo [inbox]" entry
                                 (file+headline "~/org/Work/gtd/dins-inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/org/gtd/tickler.org" "Tickler")
                                 "* %i%? \n %U")
                                ("j" "Work Journal Note" ; not quite same os org-journal, all headings on top
                                 entry (file get-journal-file-today)
                                 "* %<%H:%M> %?" ; %<> see format-time-string
                                 :empty-lines 1)))
  (add-to-list `org-modules `ol-bibtex)
  (add-to-list `org-modules `org-habit)
  (add-to-list `org-modules `org-habit-plus ))

(general-define-key
 :keymaps 'doom-leader-notes-map
 "j k" #'org-journal-open-current-journal-file)

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
        ("C-c C-q" . vterm-quit-from-emacs-client))
  :config (evil-set-initial-state 'vterm-mode 'emacs))

(defun my/create-vterm-without-perspective ()
  "Start vterm buffer, first deleting perspective and swithcing to persp #1."
  (+workspace:delete)
  (+workspace/switch-to 0)
  (+vterm/here t)
  )

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
(defun my/create-eshell-without-perspective ()
  "Start eshell buffer, first deleting perspective and swithcing to persp #1."
  (+workspace:delete)
  (+workspace/switch-to 0)
  (+eshell/here))

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

;; (use-package! 0x0)
;; (after! embark
;;   (define-key embark-region-map (kbd "U") #'0x0-dwim)) ; probably need to wrap with `after!

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
  :load-path "~/Downloads/ensime-tng-3.0.15/lisp/"
  ;; :load-path "~/Downloads/ensime-tng-3.0.12/lisp/"
  :commands ensime-mode
  :bind
  (:map ensime-mode-map
        ("C-c <tab> g" . ensime-jump-to-definition)
        ("C-c <tab> t" . ensime-type-at-point)
        ("C-c <tab> s" . ensime-symbol-at-point)
        ("C-c <tab> i" . ensime-import-symbol-at-point)))

(advice-add #'ensime-jump-to-definition :after #'better-jumper-set-jump)

(set-company-backend! 'scala-mode
  'ensime-company)
(add-hook 'scala-mode-hook #'ensime-mode)

(use-package! org-pomodoro-third-time :init)

(use-package evil-owl
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode))

(use-package! kubel)
(use-package! kubel-evil)

(use-package! keycast)

(use-package! nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; i'm ready to give up. i guess i don't need (require 'org-cv) anywhere
;; but it's called from somewhere, and it's missing in the repo. also - things don't seem to work in easy wasy
;; and also - where the hell is my CV org file...
;; (use-package! org-cv
;;   :init (require 'ox-moderncv))

(use-package! format-all)

;; workaround for lsp-metals problem, sad life
;; https://github.com/emacs-lsp/lsp-metals/issues/84
(require `treemacs-extensions)

(setq sly-complete-symbol-function 'sly-flex-completions)

(use-package! eat
  :init
  (defun eat-quit-from-emacs-client ()
    (interactive)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) t)))
      (kill-current-buffer))
    (evil-quit))
  :bind
  (:map eat-mode-map
        ("C-c C-q" . eat-quit-from-emacs-client))
  :config
  (evil-set-initial-state 'eat-mode 'emacs)
  (add-hook 'eat-mode-hook #'(lambda () (message "Eat mode initialized!"))))

(defun my/create-eat-without-perspective ()
  "Start eshell buffer, first deleting perspective and swithcing to persp #1."
  (+workspace:delete)
  (+workspace/switch-to 0)
  (eat))

(load-file "~/.doom.d/codeium-sample-config.el")

;; note manual action to lsp-install-server
(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(defun my/rustywind-in-project-root ()
  "Running tailwindcss reformatting."
  ;; TODO - different actual commands --check-formatted --dry-run. maybe add --ignored-files from gitignore?
  ;; TODO - make it also run from 'npx rustywind' and remove PATH dependency on the binary
  (interactive)
  (let ((command (mapconcat 'identity
                            (list "rustywind ."
                                  "--custom-regex"
                                  (shell-quote-argument "className := \"([^\"]+)\"")
                                  "--write") " ")))
    (projectile-run-shell-command-in-root command)
    (revert-buffer t t)))

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)


(use-package avy
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  (global-set-key (kbd "C-'") 'avy-goto-char)
  (global-set-key (kbd "C-\"") 'avy-goto-char-timer)
  (global-set-key (kbd "M-g M-g") 'avy-goto-line)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (global-set-key (kbd "M-j") 'avy-goto-char-timer)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(setq!
 isearch-allow-motion 't)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)

(server-start)
(epa-file-enable)
