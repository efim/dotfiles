;; See
;; http://takeonrules.com/2021/08/22/ever-further-refinements-of-org-roam-usage/
;; for details on this configuration.
;;
;; See https://takeonrules.com/2021/08/23/diving-into-the-implementation-of-subject-menus-for-org-roam/
;; for a walk through of the implementation.
;;
;; A Property List of my `org-roam' capture templates.
(setq ef/org-roam-capture-templates-plist
      (list
       :personal
       '("p" "Personal" plain "%?"
	 :if-new
         (file+head
          "Personal/%<%Y%m%d%H%M%s>-${slug}.org"
          "#+title: ${title}\n#+FILETAGS: :personal: \n\n")
	 :unnarrowed t)
       :work
       '("w" "Work" plain "%?"
         :if-new
         (file+head
          "Work/%<%Y%m%d%H%M%s>-${slug}.org"
          "#+title: ${title}\n#+FILETAGS: :work: \n\n")
         :unnarrowed t)
       ))

;; A plist that contains the various org-roam subject.  Each subject
;; has a plist of :templates, :title, :name, :path-to-todo, :prefix,
;; and :group.
;;
;; The :templates defines the ;; named templates available for this subject.  See
;; `ef/org-roam-capture-templates-plist' for list of valid templates.
;;
;; The :name is the string version of the subject, suitable for
;; creating function names.
;;
;; The :title is the human readable "title-case" form of the subject.
;;
;; The :group is for `pretty-hydra-define+'
;;
;; The :prefix helps with menu key build for `pretty-hydra-define+'
;;
;; The :path-to-todo is the path to the todo file for this subject.
(setq ef/org-roam-capture-subjects-plist
      (list
       ;; The :all subject is different from the other items.
       :all (list
             ;; Iterate through all registered capture templates and
             ;; generate a list
             :templates (-non-nil (seq-map-indexed (lambda (template index)
                     (when (cl-evenp index) template))
                   ef/org-roam-capture-templates-plist))
             :name "all"
             :title "All"
             :group "All"
             :prefix "a"
             :path-to-todo "~/org/gtd/gtd.org")
       :work (list
                       :templates (list :work)
                       :name "work"
                       :title "Work"
                       :group "Projects"
                       :prefix "w"
                       :path-to-todo "~/org/Work/gtd/dins-gtd.org")
       :personal (list
                  :templates (list :personal)
                  :name "personal"
                  :title "Personal"
                  :group "Life"
                  :prefix "p"
                  :path-to-todo "~/org/gtd/gtd.org")
       ))

(cl-defun ef/org-roam-templates-for-subject (subject
                                              &key
                                              (subjects-plist ef/org-roam-capture-subjects-plist)
                                              (template-definitions-plist ef/org-roam-capture-templates-plist))
  "Return a list of `org-roam' templates for the given SUBJECT.
Use the given (or default) SUBJECTS-PLIST to fetch from the
given (or default) TEMPLATE-DEFINITIONS-PLIST."
  (let ((templates (plist-get (plist-get subjects-plist subject) :templates)))
    (-map (lambda (template) (plist-get template-definitions-plist template))
          templates)))

;; A menu of common tasks for `org-roam'.  This menu is for all subjects.
;;
;; Note the convention:
;;
;; * @ - for todo
;; * + - for capture
;; * ! - for insert
;; * ? - for find
;;
;; The `create-org-roam-subject-fns-for' presupposes those keys for
;; narrowed subjects.
(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
(defvar ef/org-subject-menu--title (with-faicon "book" "Org Subject Menu" 1 -0.05))
(pretty-hydra-define ef/org-subject-menu--all (:foreign-keys warn :title ef/org-subject-menu--title :quit-key "q" :exit t)
  (
   ;; Note: This matches at least one of the :groups in `ef/org-roam-capture-subjects-plist'
   "Life"
   ()
   ;; Note: This matches at least one of the :groups in `ef/org-roam-capture-subjects-plist'
   "Projects"
   ()
   "Org Mode"
   (("@" (lambda ()
           (interactive)
           (find-file (file-truename (plist-get (plist-get ef/org-roam-capture-subjects-plist :all) :path-to-todo))))
     "Todo…")
    ("+" ef/org-roam--all--capture     "Capture…")
    ("!" ef/org-roam--all--node-insert " ├─ Insert…")
    ("?" ef/org-roam--all--node-find   " └─ Find…")
    ("/" org-roam-buffer-toggle         "Toggle Buffer")
    ("#" ef/toggle-roam-subject-filter "Toggle Default Filter")
    )))

(cl-defmacro create-org-roam-subject-fns-for (subject
                                              &key
                                              (subjects-plist ef/org-roam-capture-subjects-plist))
  "Define the org roam SUBJECT functions and create & update hydra menus.
The functions are wrappers for `org-roam-capture',
`org-roam-node-find', `org-roam-node-insert', and `find-file'.
Create a subject specific `pretty-define-hydra' and append to the
`ef/org-subject-menu--all' hydra via the `pretty-define-hydra+'
macro.
Fetch the given SUBJECT from the given SUBJECTS-PLIST."
  (let* ((subject-plist (plist-get subjects-plist subject))
         (subject-as-symbol subject)
         (subject-title (plist-get subject-plist :title))
         (subject-name (plist-get subject-plist :name))

         ;; For todo related antics
         (todo-fn-name (intern (concat "ef/find-file--" subject-name "--todo")))
         (path-to-todo (plist-get subject-plist :path-to-todo))
         (todo-docstring (concat "Find the todo file for " subject-name " subject."))

         ;; For hydra menu related antics
         (hydra-fn-name (intern (concat "ef/org-subject-menu--" subject-name)))
         (hydra-menu-title (concat subject-title " Subject Menu"))
         (hydra-todo-title (concat subject-title " Todo…"))
         (hydra-group (plist-get subject-plist :group))
         (hydra-prefix (plist-get subject-plist :prefix))
         (hydra-kbd-prefix-todo    (concat hydra-prefix " @"))
         (hydra-kbd-prefix-capture (concat hydra-prefix " +"))
         (hydra-kbd-prefix-insert  (concat hydra-prefix " !"))
         (hydra-kbd-prefix-find    (concat hydra-prefix " ?"))

         ;; For `org-roam-capture' related antics
         (capture-fn-name (intern (concat "ef/org-roam--" subject-name "--capture")))
         (capture-docstring (concat "As `org-roam-capture' but scoped to " subject-name
                            ".\n\nArguments GOTO and KEYS see `org-capture'."))

         ;; For `org-roam-insert-node' related antics
         (insert-fn-name (intern (concat "ef/org-roam--" subject-name "--node-insert")))
         (insert-docstring (concat "As `org-roam-insert-node' but scoped to " subject-name " subject."))

         ;; For `org-roam-find-node' related antics
         (find-fn-name (intern (concat "ef/org-roam--" subject-name "--node-find")))
         (find-docstring (concat "As `org-roam-find-node' but scoped to "
                            subject-name " subject."
                            "\n\nArguments INITIAL-INPUT and OTHER-WINDOW are from `org-roam-find-mode'."))
         )
    `(progn
       (defun ,todo-fn-name ()
         ,todo-docstring
         (interactive)
         (find-file (file-truename ,path-to-todo)))

       (defun ,capture-fn-name (&optional goto keys)
         ,capture-docstring
         (interactive "P")
         (org-roam-capture goto         ; so here is capture.. where does it put/look for nodes? It doesn't separate into directories, right?
                           keys
                           :filter-fn (lambda (node) (-contains-p (org-roam-node-tags node) ,subject-name))
                           :templates (ef/org-roam-templates-for-subject ,subject-as-symbol)))
       (defun ,insert-fn-name ()
         ,insert-docstring
         (interactive)
         (org-roam-node-insert (lambda (node) (-contains-p (org-roam-node-tags node) ,subject-name))
                               :templates (ef/org-roam-templates-for-subject ,subject-as-symbol)))

       (defun ,find-fn-name (&optional other-window initial-input)
         ,find-docstring
         (interactive current-prefix-arg)
         (org-roam-node-find other-window
                             initial-input
                             (lambda (node) (-contains-p (org-roam-node-tags node) ,subject-name))
                             :templates (ef/org-roam-templates-for-subject ,subject-as-symbol)))

       ;; Create a hydra menu for the given subject
       (pretty-hydra-define ,hydra-fn-name (:foreign-keys warn :title ef/org-subject-menu--title :quit-key "q" :exit t)
         (
          ,hydra-menu-title
          (
           ("@" ,todo-fn-name        ,hydra-todo-title)
           ("+" ,capture-fn-name     " ├─ Capture…")
           ("!" ,insert-fn-name      " ├─ Insert…")
           ("?" ,find-fn-name        " └─ Find…")
           ("/" org-roam-buffer-toggle            "Toggle Buffer")
           ("#" ef/toggle-roam-subject-filter    "Toggle Filter…")
           )))

       ;; Append the following menu items to the `ef/org-subject-menu--all'
       (pretty-hydra-define+ ef/org-subject-menu--all()
         (,hydra-group
          (
           (,hydra-kbd-prefix-todo    ,todo-fn-name    ,hydra-todo-title)
           (,hydra-kbd-prefix-capture ,capture-fn-name " ├─ Capture…")
           (,hydra-kbd-prefix-insert  ,insert-fn-name  " ├─ Insert…")
           (,hydra-kbd-prefix-find    ,find-fn-name    " └─ Find…")
           )))
       )))

;; I tried using a dolist to call each of the macros, but that didn't
;; work.  I'd love some additional help refactoring this.  But for
;; now, what I have is quite adequate.  It would be nice to
;; more programatically generate the hydra menus (see below).
(create-org-roam-subject-fns-for :personal)
(create-org-roam-subject-fns-for :work)

;; Including the aliases to reduce switching necessary for re-mapping
;; keys via `ef/toggle-roam-subject-filter'.
(defalias 'ef/org-roam--all--node-insert 'org-roam-node-insert)
(defalias 'ef/org-roam--all--node-find 'org-roam-node-find)
(defalias 'ef/org-roam--all--capture 'org-roam-capture)

(cl-defun ef/subject-list-for-completing-read (&key
                                                (subjects-plist
                                                 ef/org-roam-capture-subjects-plist))
  "Create a list from the SUBJECTS-PLIST for completing read.
The form should be '((\"all\" 1) (\"hesburgh-libraries\" 2))."
  ;; Skipping the even entries as those are the "keys" for the plist,
  ;; the odds are the values.
  (-non-nil (seq-map-indexed (lambda (subject index)
                     (when (cl-oddp index) (list (plist-get subject :name) index)))
                   subjects-plist)))

(require `core-keybinds)
(defun ef/toggle-roam-subject-filter (subject)
  "Prompt for a SUBJECT, then toggle the 's-i' kbd to filter for that subject."
  (interactive (list
                (completing-read
                 "Project: " (ef/subject-list-for-completing-read))))
  (map! (:leader (:prefix "n"
                  (:prefix-map ("r" . "roam")
                   :desc "Org Roam Capture"              "c" (intern (concat "ef/org-roam--" subject "--capture")) ;; #'org-roam-capture
                   :desc "Find file"                     "f" (intern (concat "ef/org-roam--" subject "--node-find")) ;; #'org-roam-node-find
                   :desc "Insert"                        "i" (intern (concat "ef/org-roam--" subject "--node-insert")) ;; #'org-roam-node-insert
                   :desc "Subj Menu"                     "m" (intern (concat "ef/org-subject-menu--" subject "/body")) ;; #'org-roam-node-insert
                   :desc "Org Roam"                      "r" #'org-roam-buffer-toggle
                   :desc "Tag"                           "t" #'org-roam-tag-add
                   :desc "Un-tag"                        "T" #'org-roam-tag-delete))))
  ;; (global-set-key
  ;;  (kbd "C-s-1")
  ;;  (intern (concat "ef/org-roam--" subject "--node-insert")))
  ;; (global-set-key
  ;;  (kbd "C-s-=")
  ;;  (intern (concat "ef/org-roam--" subject "--capture")))
  ;; (global-set-key
  ;;  (kbd "C-s-/")
  ;;  (intern (concat "ef/org-roam--" subject "--node-find")))
  ;; (global-set-key
  ;;  (kbd "s-i")
  ;;  (intern (concat "ef/org-roam--" subject "--node-insert")))
  (global-set-key
   (kbd "C-c i")
   (intern (concat "ef/org-subject-menu--" subject "/body")))
  )

;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
(use-package! org-roam
  :custom
  (org-roam-directory (file-truename "~/org"))
  ;; Set more spaces for tags; As much as I prefer the old format,
  ;; this is the new path forward.
  (org-roam-node-display-template "${title:*} ${tags:40}")
  (org-roam-capture-templates (ef/org-roam-templates-for-subject :all))
  :init
  ;; Help keep the `org-roam-buffer', toggled via `org-roam-buffer-toggle', sticky.
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\#"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))

  (setq org-roam-completion-everywhere t)
  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode)
  ;; Configure the "all" subject key map
  (ef/toggle-roam-subject-filter "all"))

(provide 'testing-roam-config-w-hydra)
;;; testing-roam-config-w-hydra.el ends here
