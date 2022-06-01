;;; testing-roam-w-transient.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Doe
;;
;; Author: John Doe <john@doe.com>
;; Maintainer: John Doe <john@doe.com>
;; Created: мая 31, 2022
;; Modified: мая 31, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/efim/testing-roam-w-transient
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;; templates are already in memory
(require `dash)

(defvar ef/roam-context :all
  "For toggling the context for the capture, insert, find.")

(defun ef/roam-saved-context ()
  "Return context set up by 'ef/roam-context'."
  (plist-get ef/org-roam-capture-subjects-plist ef/roam-context))

(cl-defun ef/roam-templates-for-context (context
                                              &key
                                              (template-definitions-plist ef/org-roam-capture-templates-plist))
  "Return a list of `org-roam' templates for the given SUBJECT.
Use the given (or default) SUBJECTS-PLIST to fetch from the
given (or default) TEMPLATE-DEFINITIONS-PLIST."
  (let ((templates (plist-get context :templates)))
    (-map (lambda (template) (plist-get template-definitions-plist template))
          templates)))

;; setq ef/org-roam-capture-templates-plist ; these are templates
;; setq ef/org-roam-capture-subjects-plist ; these are all subjects

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

(defun ef/roam-subject-by-key (key)
  "Not sure is this is needed, just `plist-get' from list of contextx by KEY."
  (plist-get ef/org-roam-capture-subjects-plist key))


;;; building the transient

;; building menu with infixes for each context
(defun ef/roam-conext-to-infix (context key)
  (let ((char (plist-get context :prefix)))
    `(,char "" ,(pp-to-string key))))

;; (ef/roam-conext-to-infix (plist-get ef/org-roam-capture-subjects-plist :work))

(setq ef/roam-infix-contexts-list (--map
  (ef/roam-conext-to-infix (plist-get ef/org-roam-capture-subjects-plist it) it)
  (ef/roam-subjects-keys)))

;; "incompatible contexts"
(setq ef/roam-incompatible-contexts-list (--map
  (pp-to-string it)
  (ef/roam-subjects-keys)))

(setq ef/roam-contexts-row-group (seq--into-vector (append `(:class transient-row "Contexts") ef/roam-infix-contexts-list)))
;; suffix commands that take selected context for call

;;; so, i'd get symbols for suffix commands from wrapped things
;;; and group them

;;; function to take transient-arg-value and return the context
(defun ef/roam-context-from-transient-arg-value ()
  "Return the context corresponding to 'transient-args' of main transient.
If no switch is set return nil."
  (interactive)
  (let ((args (transient-args
               'ef/roam-things-transient)))
    (cl-loop for arg in args
     return
     (ef/roam-subject-by-key
      (ef/roam-subject-clean-key arg)))))

;; org-roam-node-find
(cl-defun ef/roam-node-find-wrapped-with-context (input-context &optional other-window initial-input)
"Call ROAM-FUNCTION with filter-fn and templates from CONTEXT.
OTHER-WINDOW and INITIAL-INPUT passed as is."
  (interactive)
  (let* ((context (if input-context input-context (ef/roam-saved-context)))
         (subject-name (plist-get context :name))
         (filter-fn (plist-get context :filter-fn))
         (templates (ef/roam-templates-for-context context)))
    (org-roam-node-find other-window initial-input filter-fn :templates templates)))

;; org-roam-node-insert
(cl-defun ef/roam-node-insert-wrapped-with-context (input-context)
"Call ROAM-FUNCTION with filter-fn and templates from CONTEXT.
OTHER-WINDOW and INITIAL-INPUT passed as is."
  (interactive)
  (let* ((context (if input-context input-context (ef/roam-saved-context)))
         (subject-name (plist-get context :name))
         (filter-fn (plist-get context :filter-fn))
         (templates (ef/roam-templates-for-context context)))
    (org-roam-node-insert filter-fn :templates templates)))

;; org-roam-capture
(cl-defun ef/roam-capture-wrapped-with-context (input-context)
"Call ROAM-FUNCTION with filter-fn and templates from CONTEXT.
OTHER-WINDOW and INITIAL-INPUT passed as is."
  (interactive)
  (let* ((context (if input-context input-context (ef/roam-saved-context)))
         (subject-name (plist-get context :name))
         (filter-fn (plist-get context :filter-fn))
         (templates (ef/roam-templates-for-context context)))
    (org-roam-capture :templates templates :filter-fn filter-fn)))

;; now, wrapper that optionally accepts context?
;; (ef/wrap-roam-function-with-context #'org-roam-node-find :work)
;; (ef/wrap-roam-function-with-context #'org-roam-node-find ":work")
;; (ef/wrap-roam-function-with-context #'org-roam-node-find :personal)
;; (ef/wrap-roam-function-with-context #'org-roam-node-find :personal)
;; (ef/wrap-roam-function-with-context #'org-roam-node-find ":all")
;; TODO - put filter-fn into the plist describing context

;; roam-capture roam-node-insert roam-node-find
(transient-define-suffix ef/roam-contexed-node-find (&optional OTHER-WINDOW INITIAL-INPUT)
  "Wrapping `roam-node-find` with context"
  (interactive)
  (ef/roam-node-find-wrapped-with-context (ef/roam-context-from-transient-arg-value)))


(transient-define-suffix ef/roam-contexed-node-insert (&optional OTHER-WINDOW INITIAL-INPUT)
  "Wrapping `roam-node-insert` with context"
  (interactive)
  (ef/roam-node-insert-wrapped-with-context (ef/roam-context-from-transient-arg-value)))


(transient-define-suffix ef/roam-contexed-capture (&optional OTHER-WINDOW INITIAL-INPUT)
  "Wrapping `roam-capture` with context"
  (interactive)
  (ef/roam-capture-wrapped-with-context (ef/roam-context-from-transient-arg-value)))

(setq ef/roam-actions-transient-suffixes `[
   "Actions"
   ("?" "find" ef/roam-contexed-node-find)
   ("i" "insert" ef/roam-contexed-node-insert)
   ("c" "capture" ef/roam-contexed-capture)
   ,ef/roam-show-args-transient-suffix
   ])
;; suffix command that sets default context

;; prefix command that groups these

(setq ef/roam-show-args-transient-suffix `("=" "show args" (lambda ()
                                                             "my increase width"
                                                             (interactive)
                                                             ;; (print (transient-args 'ef/roam-things-transient))
                                                             ;; (print (transient-arg-value ":personal" (transient-args 'ef/roam-things-transient)))
                                                             ;; (print (transient-arg-value ":work" (transient-args 'ef/roam-things-transient)))
                                                             ;; (print (transient-arg-value ":all" (transient-args 'ef/roam-things-transient)))
                                                             (print (ef/roam-context-from-transient-arg-value))
                                                             )))

(eval `(transient-define-prefix ef/roam-things-transient ()
         "Lalala."
         :incompatible (list (list . ,ef/roam-incompatible-contexts-list)) ; there has to be a better way to get quoted list from a function with map
         [
          :class transient-subgroups
          ef/roam-contexts-row-group
          ]
         ,ef/roam-actions-transient-suffixes
         ;; ,(seq--into-vector (append `("Contexts") ef/roam-infix-contexts-list))
         ))

(ef/roam-things-transient)

(provide 'testing-roam-w-transient)
;;; testing-roam-w-transient.el ends here
