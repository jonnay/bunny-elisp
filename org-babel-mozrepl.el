;;; org-babel-mozrepl.el --- A Firefox interface into org-babel
(defconst org-babel-mozrepl-version "0.1")
;; Copyright (c)2010 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
;;; Installation:
;; Put org-babel-mozrepl.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file.
;(require 'org-babel-mozrepl)


;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; TODO:
;; 

;;; CHANGELOG:
;; v 0.1 - Initial release

;;; Code:

(require 'org-babel)
(require 'moz)

(defvar org-babel-moz-eoe-indicator "\"org-babel-moz-js-eoe\";")
(defvar org-babel-moz-eoe-output "org-babel-moz-js-eoe")

(org-babel-add-interpreter "moz-js")

(add-to-list 'org-babel-tangle-langs '("moz-js" "js"))

(defun org-babel-expand-body:moz-js (body params &optional processed-params)
  "Expand"
  (message "Expanding Body.  body: %S params: %S proc-params: %S" body params processed-params)
  (let ((vars (second (or processed-params (org-babel-process-params params)))))
	(concat
	 (mapconcat
	  (lambda (pair) (format "var %s = %s;"
							 (car pair)
							 (cdr pair)))
	  vars
	  "\n")
	 "\n" body "\n")))

(defun org-babel-execute:moz-js (body params)
  "Execute a body"
  (message "Executing Mozilla Javascript.")
  (let* ((processed-params (org-babel-process-params params))
		 (vars (second processed-params))
		 (full-body (org-babel-expand-body:moz-js body params processed-params))
		 (session (org-babel-prep-session:moz-js params))
		 (raw (org-babel-comint-with-output session org-babel-moz-eoe-output nil
											;(insert (concat moz-repl-name ".pushenv('printPrompt');"))
											;(comint-send-input nil t)
											;(insert (concat moz-repl-name ".setenv('printPrompt', false);"))
											(insert (concat (org-babel-chomp full-body) ";"))
											(comint-send-input nil t)
											(insert org-babel-moz-eoe-indicator)
											(comint-send-input nil t)
											(insert ";")
											(comint-send-input nil t)
											;(insert (concat moz-repl-name ".popenv('printPrompt');"))
											(comint-send-input nil t))))
	(message "%S" raw)
	(org-babel-reassemble-table
	 (org-babel-moz-parse-output raw)
	 (org-babel-pick-name (nth 4 processed-params) (cdr (assoc :colnames params)))
	 (org-babel-pick-name (nth 5 processed-params) (cdr (assoc :rownames params))))))

(defun org-babel-prep-session:moz-js (params)
  "Prep Session for execution"
  (message "params %S"  params)
  (buffer-name (process-buffer (inferior-moz-process))))

(defun org-babel-moz-parse-output (output)
  "Parse the output from the mozrepl buffer"
  (message "Parse output was %S" output)
  (cadr output))



;; Due to improper abstraction, we need to repeat some code from moz.el
(defun org-babel-moz-send (body)
  )



(when nil
	   (comint-send-string (inferior-moz-process)
						   (concat moz-repl-name ".pushenv('printPrompt', 'inputMode'); "
								   moz-repl-name ".setenv('printPrompt', false); "
								   moz-repl-name ".setenv('inputMode', 'multiline'); "
								   "undefined; \n"))
	   ;; Give the previous line a chance to be evaluated on its own.  If
	   ;; it gets concatenated to the following ones, we are doomed.
	   (sleep-for 0 1)
	   (comint-send-region (inferior-moz-process)
						   start end)
	   (comint-send-string (inferior-moz-process)
						   "\n--end-remote-input\n")
	   (comint-send-string (inferior-moz-process)
						   (concat moz-repl-name ".popenv('inputMode', 'printPrompt'); "
								   "undefined; \n"))
	   (comint-send-string (inferior-moz-process)
						   "\n--end-remote-input\n")
	   (display-buffer (process-buffer (inferior-moz-process))))

(provide 'org-babel-mozrepl)

;;; org-babel-mozrepl ends here
