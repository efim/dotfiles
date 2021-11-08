;;; app-sys-notify.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Doe
;;
;; Author: John Doe <https://github.com/efim>
;; Maintainer: John Doe <john@doe.com>
;; Created: ноября 05, 2021
;; Modified: ноября 05, 2021
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'notifications)
(require 'appt)

;; TODO wrapper funciton that parses parameters as they come into ~appt-display-message~
;;      and execute inner function accordingly
;; TODO rewrite as inner function that takes single param

(defun my/appt-notifications-notify (&rest appt-display-message-args)
  "To be used as :before advice for forwarding notifications to the system, displaying contents of APPT-DISPLAY-MESSAGE-ARGS."
  (let* ((appt-message (car appt-display-message-args))
         (due-in-minutes (cadr appt-display-message-args))
         (message-to-display (format "Org Appointment:\n%s\nin %s min." appt-message due-in-minutes))
         ;; (urgency (cond ((<= due-in-minutes appt-display-interval) 'critical)
         ;; (t 'normal)))
         )
    (notifications-notify :body message-to-display :timeout 60000 :urgency 'normal)))

;; using:
;; (advice-add #'appt-display-message :after #'my/appt-notifications-notify)
;; removing:
;; (advice-remove #'appt-display-message #'my/appt-notifications-notify)

(provide 'app-sys-notify)
;;; app-sys-notify.el ends here
