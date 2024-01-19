;;; karthink-avy-actions.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 John Doe
;;
;; Author: John Doe <john@doe.com>
;; Maintainer: John Doe <john@doe.com>
;; Created: January 03, 2024
;; Modified: January 03, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/enefedov/karthink-avy-actions.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

(provide 'karthink-avy-actions)
;;; karthink-avy-actions.el ends here





