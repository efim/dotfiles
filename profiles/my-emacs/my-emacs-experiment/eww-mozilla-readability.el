
;; copied from the
;; https://lifeofpenguin.blogspot.com/2024/04/mozilla-readability-in-gnu-emacs.html?m=1
(defun mozilla-readability (start end pageAddress)
  (let ((command (concat "readable --base " (or pageAddress "localhost"))))
	 (message "readability with address %s" pageAddress)
	 (shell-command-on-region start end command nil t)))

;; owerriding existing function
;; by default bound to R in eww-mode, so enable manually
(defun eww-readable ()
  "View the main \"readable\" parts of the current web page.
This command uses heuristics to find the parts of the web page that
contains the main textual portion, leaving out navigation menus and
the like."
  (interactive nil eww-mode)
  (let* ((old-data eww-data)
	 (dom (with-temp-buffer
		(insert (plist-get old-data :source))
		(condition-case nil
		    (decode-coding-region (point-min) (point-max) 'utf-8)
		  (coding-system-error nil))
		(mozilla-readability  (point-min) (point-max) (plist-get old-data :url)) ; this is the added part
      (eww--preprocess-html (point-min) (point-max))
		(libxml-parse-html-region (point-min) (point-max))))
         (base (plist-get eww-data :url)))
    (eww-score-readability dom)
    (eww-save-history)
    (eww-display-html nil nil
                      (list 'base (list (cons 'href base))
                            (eww-highest-readability dom))
		      nil (current-buffer))
    (dolist (elem '(:source :url :title :next :previous :up :peer))
      (plist-put eww-data elem (plist-get old-data elem)))
    (eww--after-page-change)))

