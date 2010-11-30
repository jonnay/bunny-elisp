;;; org-pocketmod.el --- An extension to org printing to make a pocketmod
(defconst org-pocketmod-version "0.1")
;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
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
(defgroup org-pocketmod '()
  "Post-process your print-outs to turn then into mini-books.

A pocket mod is a method of folding and cutting a sheet of paper so
you end up with a mini 8-page book.  In the lifehacker/GTD community
it is a popular way to print out a custom agenda.

Based off of a shellscript from Has de Zwart.")

;; This could also use pstops...
;; http://knackered.knackered.org/angus/psutils/pstops.html

;;; Installation:
;; Put org-pocketmod.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file.
;(require 'org-pocketmod)


;;; TODO:
;; 

;;; CHANGELOG:
;; v 0.1 - Initial release

;;; Code:

(defcustom org-pocketmod-mogrify "mogrify"
  "Command to resample and rotate images."
  :type 'file
  :group 'org-pocketmod)
  
(defcustom org-pocketmod-convert "convert"
  "Command to turn PDF files into PNGs"    
  :type 'file
  :group 'org-pocketmod)

(defun org-pocketmod-gen-temp ()
  "Generate a temporary file name for org-pocketmod"
  (format "org-pocketmod-temp-%d" (random 99999999)))

(defvar org-pocketmod-tempfile-prefix (org-pocketmod-gen-temp))

(defvar org-temp-files '()
  "A list of temporary files we have created, so we can delete them later.")

(defun org-pocketmod (input output)
  "Transform a ps file into a pocketmod version, suitable for 8-up printing."
  (interactive "fInput File: \nFOutput File:\n")
  (org-pocketmod-build  input  output))

(defun org-pocketmod-build (input output)
  "Build a pocketmod from the input file."
  (org-pocketmod-convert input (org-pocketmod-get-file "-%d.png"))
  (org-pocketmod-shrink-rotate 'left (org-pocketmod-get-file "-0.png"))
  (org-pocketmod-shrink-rotate 'right (org-pocketmod-get-file "-1.png"))
  (org-pocketmod-shrink-rotate 'right (org-pocketmod-get-file "-2.png"))
  (org-pocketmod-shrink-rotate 'right (org-pocketmod-get-file "-3.png"))
  (org-pocketmod-shrink-rotate 'right (org-pocketmod-get-file "-4.png"))
  (org-pocketmod-shrink-rotate 'left (org-pocketmod-get-file "-5.png"))
  (org-pocketmod-shrink-rotate 'left (org-pocketmod-get-file "-6.png"))
  (org-pocketmod-shrink-rotate 'left (org-pocketmod-get-file "-7.png"))
  (org-pocketmod-stitch (org-pocketmod-get-file "-row1.png")
						(org-pocketmod-get-file "-0.png")
						(org-pocketmod-get-file "-1.png"))
  (org-pocketmod-stitch (org-pocketmod-get-file "-row2.png")
						(org-pocketmod-get-file "-7.png")
						(org-pocketmod-get-file "-2.png"))
  (org-pocketmod-stitch (org-pocketmod-get-file "-row3.png")
						(org-pocketmod-get-file "-6.png")
						(org-pocketmod-get-file "-3.png"))
  (org-pocketmod-stitch (org-pocketmod-get-file "-row4.png")
						(org-pocketmod-get-file "-5.png")
						(org-pocketmod-get-file "-4.png"))
  (org-pocketmod-stitch (org-pocketmod-get-file "-total.png")
						(org-pocketmod-get-file "-row1.png")
						(org-pocketmod-get-file "-row2.png")
						(org-pocketmod-get-file "-row3.png")
						(org-pocketmod-get-file "-row4.png"))
  (org-pocketmod-convert (org-pocketmod-get-file "-total.png") output)
  (org-pocketmod-clean-temporary-files))

(defun org-pocketmod-blank (filename)
  (org-pocketmod-cmd org-pocketmod-convert
					 "-size"
					 "100x100"
					 "xc:white"
					 filename))

(defun org-pocketmod-clean-temporary-files ()
  "Clean out any temporary files.  Dummy function for now."
  nil)

(defun org-pocketmod-cmd (cmd &rest args)
  "Run a command through the shell.

There is going to have to be some kind of argument munging.  For now everything runs through start process."
  ;; start-process does things asynchronously which is not what we needs. 
  ;;(apply 'start-process "org-pocketmod" (get-buffer-create "*org-pocketmod-output") cmd args)
  (let ((out-buffer (get-buffer-create "*org-pocketmod-output*")))
	(save-excursion
	 (set-buffer out-buffer)
	 (insert (format "Calling %s\n    with args %s\n" cmd args))
	 (apply 'call-process-shell-command cmd nil out-buffer t args))))

(defun org-pocketmod-get-file (file)
  "Returns a fully pathed temporary file, and stores it in the list of temp files."
  (format "%s%s%s" temporary-file-directory org-pocketmod-tempfile-prefix file))

;; use temporary-file-directory for filepaths 
(defun org-pocketmod-shrink-rotate (orientation file)
  "Rotate FILE according to ORIENTATION

ORIENTATION should be a symbol being either 'left or 'right for
-90 or +90 degrees
FILE is automagickally prefixed.

if FILE does not exist, a whole new file is created."
  (if (file-exists-p file)
	  (org-pocketmod-cmd org-pocketmod-mogrify
						 "-sample"
						 "800%"
						 "-resize"
						 "12.5%"
						 "-rotate"			
						 (case orientation
						   ((left) "-90")
						   ((right) "90")
						   (t (error "Unknown orientation in org-pocketmod-mogrify!  Should be 'left or 'right.")))
						 file)
	  (org-pocketmod-blank file)))

(defun org-pocketmod-convert (input output)
  "Convert INPUT file into OUTPUT"
  (apply 'org-pocketmod-cmd org-pocketmod-convert (list "-verbose" input output)))

(defun org-pocketmod-stitch (output &rest files)
  "Stitch together FILES into OUTPUT."
  (apply 'org-pocketmod-cmd org-pocketmod-convert (cons "-append" files)))

(when nil
	  (progn
	   (eval-buffer)
	   (save-excursion
		(pop-to-buffer "*org-pocketmod-output*")
		(erase-buffer)
		(org-pocketmod "~/../Desktop/pocketmod.ps" "~/../Desktop/pocketmod-8.ps"))))

(provide 'org-pocketmod)

;;; org-pocketmod ends here
"
# # !/bin/bash  
#  
# # This scipt takes an 8 page PDF as an argument and turns it into a single page PDF Pocketmod  
# # It makes the following assumptions:  
# # * First paramater is the name of an 8 page PDF file  
# # * Second parameter is the name of the output file  
# #  
# # Example usage:   
# #  
# # ./pocketmod.sh input.pdf output.pdf  
#   
#  
# # convert the PDF to individual PNG's  
# echo \"Starting the conversion of $1\"  
# convert $1 %d.png  
#  
# # Manipulate each PNG as necessary  
# echo \"Working on page 1...\"  
# mogrify -sample 800% -resize 12.5% -rotate -90 0.png   
# echo \"Working on page 2...\"  
# mogrify -sample 800% -resize 12.5% -rotate 90 1.png  
# echo \"Working on page 3...\"  
# mogrify -sample 800% -resize 12.5% -rotate 90 2.png  
# echo \"Working on page 4...\"  
# mogrify -sample 800% -resize 12.5% -rotate 90 3.png  
# echo \"Working on page 5...\"  
# mogrify -sample 800% -resize 12.5% -rotate 90 4.png  
# echo \"Working on page 6...\"  
# mogrify -sample 800% -resize 12.5% -rotate -90 5.png  
# echo \"Working on page 7...\"  
# mogrify -sample 800% -resize 12.5% -rotate -90 6.png  
# echo \"Working on page 8...\"  
# mogrify -sample 800% -resize 12.5% -rotate -90 7.png  
#  
# # Tile them together  
# echo \"Creating the first row...\"  
# convert +append 0.png 1.png row1.png  
# echo \"Creating the second row\"  
# convert +append 7.png 2.png row2.png  
# echo \"Creating the third row\"  
# convert +append 6.png 3.png row3.png  
# echo \"Creating the fourth row\"  
# convert +append 5.png 4.png row4.png  
# echo \"Assembling the complete document ...\"  
# convert -append row1.png row2.png row3.png row4.png total1.png  
#  
# # Resize the whole thing  
# echo \"Outputting to $2\"  
# convert total1.png $2  
#  
# # clean up the mess  
# rm *.png  
# echo \"Done! Kind regards, Hans de Zwart\" 
"