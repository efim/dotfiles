;;; testing-roam-w-transient.el --- Description -*- lexical-binding: t; -*-
(require `dash)
(require `transient)
(require `org-roam)

;; Current implementation is with transient, not hydra as in inspirational article.
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
       :general
       '("g" "General" plain "%?"
	 :if-new
         (file+head
          "%<%Y%m%d%H%M%s>-${slug}.org"
          "#+title: ${title}\n\n")
	 :unnarrowed t)
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
;; has a plist of :templates, :path-to-todo, :prefix
;;
;; The :templates defines the ;; named templates available for this subject.  See
;; `ef/org-roam-capture-templates-plist' for list of valid templates.
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
             :prefix "a"
             :filter-fn (lambda (node) t)
             :path-to-todo "~/org/gtd/gtd.org")
       :work (list
                       :templates (list :work)
                       :prefix "w"
                       :filter-fn (lambda (node) (-contains-p (org-roam-node-tags node) "work"))
                       :path-to-todo "~/org/Work/gtd/dins-gtd.org")
       :personal (list
                  :templates (list :personal :general)
                  :prefix "p"
                  :filter-fn (lambda (node) (-contains-p (org-roam-node-tags node) "personal"))
                  :path-to-todo "~/org/gtd/gtd.org")
       ))

(defvar ef/roam-context :all
  "For toggling the context for the capture, insert, find.")

(cl-defun ef/roam-templates-for-context (context-key)
  "Return a list of `org-roam' templates for the subject by provided CONTEXT-KEY."
  (-let [(&plist context-key (&plist :templates)) ef/org-roam-capture-subjects-plist]
    (-map (-partial #'plist-get ef/org-roam-capture-templates-plist) templates)))

(defun ef/roam-subjects-keys ()
  "All keys for configured subjects."
  (doom-plist-keys ef/org-roam-capture-subjects-plist))

(defun ef/roam-subject-clean-key (key)
  "Get roam context key as object from KEY."
  (pcase key
    ((pred symbolp) key)
    ((pred stringp) (intern key))
    ;; default
    (x (error! "Can't treat %s as key" x))))

;;; building the transient

;; building menu with infixes for each context
(defun ef/roam-conext-to-infix (context key)
  (let ((char (plist-get context :prefix)))
    `(,char "" ,(pp-to-string key))))

(setq ef/roam-infix-contexts-list (--map
  (ef/roam-conext-to-infix (plist-get ef/org-roam-capture-subjects-plist it) it)
  (ef/roam-subjects-keys)))

;; "incompatible contexts"
(setq ef/roam-incompatible-contexts-list (-map #'pp-to-string (ef/roam-subjects-keys)))

(setq ef/roam-contexts-row-group (seq--into-vector (append `(:class transient-row "Contexts") ef/roam-infix-contexts-list)))
;; suffix commands that take selected context for call

;;; so, i'd get symbols for suffix commands from wrapped things
;;; and group them

;;; function to take transient-arg-value and return the context
(defun ef/roam-context-from-transient-arg-value ()
  "Return the context corresponding to 'transient-args' of main transient.
If no switch is set return nil."
  (interactive)
  (-let [(first-arg) (transient-args 'ef/roam-things-transient)]
    (ef/roam-subject-clean-key first-arg)))

;; org-roam-node-find
(cl-defun ef/roam-node-find-wrapped-with-context (&optional other-window initial-input &key (context-key nil))
  "Call ROAM-FUNCTION with filter-fn and templates from CONTEXT.
OTHER-WINDOW and INITIAL-INPUT passed as is."
  (interactive)
  (-let* ((context-key (or context-key ef/roam-context))
          ((&plist context-key (&plist :filter-fn)) ef/org-roam-capture-subjects-plist)
           (templates (ef/roam-templates-for-context context-key)))
      (org-roam-node-find other-window initial-input filter-fn nil :templates templates)))

;; org-roam-node-insert
(cl-defun ef/roam-node-insert-wrapped-with-context (&key (context-key nil))
"Call ROAM-FUNCTION with filter-fn and templates from CONTEXT.
OTHER-WINDOW and INITIAL-INPUT passed as is."
  (interactive)
  (-let* ((context-key (or context-key ef/roam-context))
         ((&plist context-key (&plist :filter-fn)) ef/org-roam-capture-subjects-plist)
         (templates (ef/roam-templates-for-context context-key)))
    (org-roam-node-insert filter-fn :templates templates)))

;; org-roam-capture
(cl-defun ef/roam-capture-wrapped-with-context (&optional goto keys &key (context-key nil))
"Call ROAM-FUNCTION with filter-fn and templates from CONTEXT.
OTHER-WINDOW and INITIAL-INPUT passed as is."
  (interactive)
  (-let* ((context-key (or context-key ef/roam-context))
         ((&plist context-key (&plist :filter-fn)) ef/org-roam-capture-subjects-plist)
         (templates (ef/roam-templates-for-context context-key)))
    (org-roam-capture goto keys :templates templates :filter-fn filter-fn)))

;; for visiting todo configured for context
(defun ef/roam-visit-todo-wrapped-with-context (&optional context-key)
"Call ROAM-FUNCTION with filter-fn and templates from CONTEXT.
OTHER-WINDOW and INITIAL-INPUT passed as is."
  (interactive)
  (-let* ((context-key (or context-key ef/roam-context))
         ((&plist context-key (&plist :path-to-todo)) ef/org-roam-capture-subjects-plist))
    (find-file path-to-todo)))

;; roam-capture roam-node-insert roam-node-find
(transient-define-suffix ef/roam-contexed-node-find ()
  "Wrapping `roam-node-find` with context"
  (interactive)
  (ef/roam-node-find-wrapped-with-context nil "" :context-key (ef/roam-context-from-transient-arg-value)))

(transient-define-suffix ef/roam-contexed-node-insert ()
  "Wrapping `roam-node-insert` with context"
  (interactive)
  (ef/roam-node-insert-wrapped-with-context :context-key (ef/roam-context-from-transient-arg-value)))

(transient-define-suffix ef/roam-contexed-capture ()
  "Wrapping `roam-capture` with context"
  (interactive)
  (ef/roam-capture-wrapped-with-context nil nil :context-key (ef/roam-context-from-transient-arg-value)))

;; suffix command that sets default context
(defun ef/roam-save-context-from-transient-args ()
  "Setting global org-roam context from transient state."
  (interactive)
  (-let [(first-key) (transient-args 'ef/roam-things-transient)]
    (pcase (ef/roam-subject-clean-key first-key)
      ((pred not) (message (format "No context selected, keeping state %s" ef/roam-context)))
      ((pred (funcall (-partial #'equal ef/roam-context))) (message (format "Same context selected, keeping state %s" ef/roam-context)))
      ;; default
      (key (if (yes-or-no-p (format "Do you want to set global roam context to %s?" key))
             (progn
               (set-variable 'ef/roam-context key)
               (message (format "New saved roam context: %s" ef/roam-context)))
           (message (format "Cancelled. Keeping state %s" ef/roam-context)))))))

(setq ef/roam-actions-transient-suffixes `[
   :description (lambda () (format "Current default context: %s" ef/roam-context))
   :class transient-row
   "Actions"
   ("?" "find" ef/roam-contexed-node-find)
   ("i" "insert" ef/roam-contexed-node-insert)
   ("c" "capture" ef/roam-contexed-capture)
   ("@" "visit todo" (lambda () "doc" (interactive) (ef/roam-visit-todo-wrapped-with-context (ef/roam-context-from-transient-arg-value))))
   ("C" "set context" ef/roam-save-context-from-transient-args)
   ])

;; prefix command that groups these

(eval `(transient-define-prefix ef/roam-things-transient ()
         "Lalala."
         :incompatible (list (list . ,ef/roam-incompatible-contexts-list)) ; there has to be a better way to get quoted list from a function with map
         [
          :class transient-subgroups
          ef/roam-contexts-row-group
         ,ef/roam-actions-transient-suffixes
          ]
         ))

;; for testing purposes
;; (ef/roam-things-transient)

;; i need not alias, but remap
(require `core-keybinds)
(map! (:leader (:prefix "n"
                  (:prefix-map ("r" . "roam")
                   :desc "Org Roam Capture"              "c" 'ef/roam-capture-wrapped-with-context ;; #'org-roam-capture
                   :desc "Find file"                     "f" 'ef/roam-node-find-wrapped-with-context ;; #'org-roam-node-find
                   :desc "Insert"                        "i" 'ef/roam-node-insert-wrapped-with-context ;; #'org-roam-node-insert
                   :desc "Subj Menu"                     "m" 'ef/roam-things-transient ;; #'org-roam-node-insert
                   :desc "Todo"                          "@" #'ef/roam-visit-todo-wrapped-with-context
                   :desc "Org Roam"                      "r" #'org-roam-buffer-toggle
                   :desc "Tag"                           "t" #'org-roam-tag-add
                   :desc "Un-tag"                        "T" #'org-roam-tag-delete))))

(general-define-key
 :keymaps 'global
 "C-c i" 'ef/roam-things-transient)

;; With the latest update of org-roam, things again behavior
;; correctly.  Now I can just load org-roam as part of my day to day
(use-package! org-roam
  :custom
  (org-roam-directory (file-truename "~/org"))
  ;; Set more spaces for tags; As much as I prefer the old format,
  ;; this is the new path forward.
  (org-roam-node-display-template "${title:*} ${tags:40}")
  (org-roam-capture-templates (ef/roam-templates-for-context ef/roam-context))
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
  (org-roam-db-autosync-mode))

(provide 'testing-roam-w-transient)

;;; testing-roam-w-transient.el ends here
