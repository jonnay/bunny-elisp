;;; free-tagging.el - Provide a tagcloud and filtering for buffers
;;*jonathan
(defconst tagging-version "0.7")
;; Public Domain
;; Original Author: Conrad Barski, M.D.
;; Modifications and Updates: Jonathan Arkell <jonnay@jonnay.net>

;; Some logic for this program was taken from hide-lines.el from Mark Hulme-Jones.
;; decended from  file at http://lisperati.com/tagging.html by Conrad Barski, M.D.

;;; Commentary:
;; free-tagging is compatable with Emacs 22 or higher only.
;;* todo document
(defgroup tagging '()
  "This package allows you to tag certian locations in a buffer.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `tagging-show-tags'
;;    Shows all the tags in a buffer.
;;  `tagging-mouse-event'
;;    Handle a mouse event.  Typically not called directly by the user.
;;  `tagging-scan-file'
;;    Scans the buffer for all tags.
;;  `tagging-jump-to-tag'
;;    Jumps to TAG in the file.
;;  `tagging-show-all'
;;    Removes all filters on the sourceode.
;;  `tagging-filter-set'
;;    Sets the tagging filter to TAGS.
;;  `tagging-filter-add'
;;    Add TAGS to the filters.
;;  `tagging-filter-subtract'
;;    Remove TAGS from teh filters.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `tagging-debug'
;;    Debug general debug output for tagging
;;    default = nil
;;  `tagging-debug-weight'
;;    Spit out relative weight after the tag
;;    default = nil
;;  `tagging-debug-wrap'
;;    Show obscenely verbose debug info for wrapping
;;    default = nil
;;  `tagging-no-tagging-on-modes'
;;    A list of modes to disable tagging on.
;;    default = (quote (info-mode erc-mode))
;;  `tagging-buffer-sync'
;;    *Synchronize the tagging buffer automatically with the current buffer.
;;    default = (quote basic)
;;  `tagging-buffer-sync-delay'
;;    *Time Emacs must be idle before the bufferinfo-buffer is synchronized.
;;    default = (quote basic)
;;  `tagging-buffer-sync-hook'
;;    Hook run at the end of `tagging-buffer-sync'.
;;    default = nil

You can search a buffer for tags, or view the buffers tag cloud.
This package has some heavy-duty ECB integration. ")

;;; Installation:
;; Put free-tagging.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file.
;   (require 'free-tagging)

;; Because each language has its own method of doing comments, you
;; might want to do something like this:
;;  (add-hook 'java-mode-hook (lambda () (setq tagging-tagline-indicator "^\s*\*\s*@tags ")
;; This will set up the java mode to use the syntax "* @tags " which should
;; fit nicely with the tagging plugin for eclipse.

;; For my setup, I went even harder core:

;; (defvar c-style-programming-modes
;;   '(c-mode c++-mode objc-mode java-mode php-mode css-mode js2-mode)
;;   "List of c-like modes")

;; (defun convert-mode-name-to-hook (mode-name)
;;   "Converts a mode name into the symbol for its hook"
;;   (intern (concat (symbol-name mode-name) "-hook")))

;; (defun add-c-style-hook (hook)
;;   "Add a hook across all c-style modes."
;;   (mapc (lambda (mode) (add-hook (convert-mode-name-to-hook mode) hook))
;; 		c-style-programming-modes))

;; (add-c-style-hook (lambda () (set (make-local-variable 'tagging-tagline-indicator) "^//\\*")))
;; (add-c-style-hook 'tagging-minor-mode)

;; ;; this will hopefully supercede the previous setting of the tagline indicator. 
;; (add-hook java-mode (lambda () (set (make-local-variable 'tagging-tagline-indicator) "@tag ")))

;; ;; this part could be done better.
;; (defun add-lispy-hook (thing)
;;   "Adds a hook to lisp-mode, emacs-lisp-mode and scheme-mode."
;;   (add-hook 'lisp-mode-hook thing)
;;   (add-hook 'emacs-lisp-mode-hook thing)
;;   (add-hook 'scheme-mode-hook thing))

;; (add-lispy-hook (lambda () (set (make-local-variable 'tagging-tagline-indicator) "^;;\\*")))

;; If you want, just select this area as a region, copy, and yank it, and uncomment, and
;; you should be good to go.

;; note that this file is chock full of tags, both as a personal proof of concept, and
;; as an example for others.  One of the coolest things about tagging code like this
;; is that you can see which code was Conrads, and which code I contributed to,
;; and which code was Conrads that I later hacked.  (filter by first name.  Filter
;; both names to see which code I modified.)


;;; TODO:
;; - Project based tagging, allow for tags to be retrieved from somewhere
;;   other then just the file.  (all tags in a directory, ec.)
;; - Anything integration
;; - tag cloud info (filtered tags, all tags)
;;   - reset button on tag cloud
;; - Tag cloud actions
;;   - right click, subtract from filter, change color of text (red) 
;; - Fix org mode tagging

;;; CHANGELOG:
;; v 0.1 - Initial release by Conrad Barski, M.D.
;; v 0.2 - Added showing of tags
;;       - Tag indicators are buffer local, so they can be changed on a per-mode
;;         per project basis
;;       - If ECB is activated, then an ecb tagging window is made.  Note that
;;         you will need to make a layout for ECB that includes that type. (tagging)
;; v 0.3 - Mis-versioned as v 0.4
;; v 0.4 - Fixed dumb startup bugs. (sorry)
;;       - Renamed the file from tagging to free-tagging, (to avoid confusion from
;;         conrads version).
;;       - Added provides
;;       - Released under the GPL
;;       - Extra customizations for debugging info
;;       - Proper word wrapping on the taglist. (It's not 100% yet)
;; v 0.5 - Added faces for subtracted and added filters on the tag cloud
;;       - fixed mouse events
;;       - renamed tagging-weight debug to tagging-debug-weight for consistency sake
;;       - fixed word wrap on tag cloud to be intelligent.  (whoohoo!)
;; v 0.6 - Fixed header-line.  (bad yasnippet)
;;       - set the point to the top of the file when a tag is clicked
;;       - first crack at making it work with org mode (doesn't quite yet)
;;       - better per-mode tagging definition handling
;;       - removed tag-definition variables to work better with per-mode tagging
;;       - made tag-line-indicator and tag-characters user-modifiable.
;;       - Fixed provide form
;; v 0.7 - Added customizeale variable so that some buffers can be removed
;;       - alphabetized the tag list.
;;       - cut down on the constant parsing and reparsing of a file.
;;       - buffers without a comment do not get parsed at all.
;;       - fixed face customization issue (Thanks Drew!)
;;       - changed height of the tag face to a little bigger (thanks Drew)
;;       - small typos fixed (thanks Drew!)
;;       - better documentation (thanks Drew!)
;;       - Fix license (strictly public domain, thanks Drew!)
;; v 0.8 - Update to work with the latest ecb version. (2.40)

;;; BUGS:
;; - when buffers are too big, they get parsed, some buffers should just not be parsed at all
;; - the keyboard actions on the view tags buffer does not work.
;; - subtraction on the tag-cloud isn't working quite right.
;; - changing a tag doesn't re-update the filters



;; For tagging(-minor)-mode, you might want to define a convention, used 
;; by default but customizable (ignorable), for determining 
;; tagging-tagline-indicator from the mode's comment-start. E.g., (format 
;; "^%s*" comment-start). Or provide a customizable alist with entries 
;; (MODE . INDICATOR), with predefined entries for common modes. Then a 
;; user need only load the library and use it.

;; Maybe add a little more to the doc strings of the commands, and  
;; mention   tagging-show-tags in the Commentary.

;; Maybe add a key binding for *-show-tags to the tagging(-minor)-mode.
;; You probably need to  
;; call pop-to-buffer or display-buffer at some point. And you probably 
;; want to pass t as the FRAME arg to get-buffer-window anyway, in case 
;; someone uses non-nil pop-up-frames, so *Tags in current buffer* is in 
;; a separate frame. Otherwise, even if the buffer is displayed, you'll 
;; get the same (= nil 0) error.



(require 'cl)

;;*macro ecb jonathan
(defmacro tagging-edit-readonly (buffer &rest body)
  "Wrap the entire section of code so that it toggles read-only.
If ecb is available, it will use the ecb macro, otherwise a
simple version."
  (if (featurep 'ecb)
	  `(ecb-with-readonly-buffer ,buffer ,@body)
	  `(save-excursion
		(set-buffer ,buffer)
		(toggle-read-only -1)
		,@body
		(toggle-read-only 1))))

;;*var tagging-definition conrad
(defvar tagging-tagline-indicator "^\\*"
  "* Regular expression that identifies the start of a tag")
(defvar tagging-tag-characters "a-zA-Z0-9_:-"
  "* Character class that describes a tag.")

;;*var depricated 
;(defvar tagging-tag-definition (concat "[" tagging-tag-characters "]+"))
;(defvar tagging-tag-input-definition (concat "-?[" tagging-tag-characters "]+"))

;;*var filter conrad
(defvar tagging-filter-cur nil)
(defvar tagging-invisible-areas ())

;;*var buffer jonathan
(defconst tagging-taglist-buffer-name
  (if (featurep 'ecb)
	  " *Tags in current buffer*"
	  "*Tags in current buffer*")
  "Name of the tagging buffer.  If ecb is activated, we need to make the buffer hidden.")

(defvar tagging-taglist-buffer (get-buffer-create tagging-taglist-buffer-name)
  "Buffer that holds the taglist")

;;*tagging-tags-hash var jonathan
(defvar tagging-tags-hash (make-hash-table)
  "Hashtable that contains the current tags for this file.
This variable is buffer local if set in any way.")
(make-variable-buffer-local 'tagging-tags-hash)


;;*var show custom jonathan
(defface tagging-taglist-face
  '((default
	  :family "helv"
	  :height 300))
  "Font face that is used for the tags listing.

Note that the height of this face is variable.  (betwen 0.25 and 1.25 of the
size defined here)."
  :group 'tagging)

(defface tagging-taglist-info-face
  '((default
       :inherit header-line))
  "Font face to display the currently selected tags"
  :group 'tagging)

(defface tagging-taglist-sub-face
  '((default
	  :foreground "firebrick"
	  :inherit tagging-taglist-face))
  "Font face for a tag that has been subtracted from the filters"
  :group 'tagging)

(defface tagging-taglist-add-face
  '((default
	  :foreground "lime green"
	  :inherit tagging-taglist-face))
  "Font face for a tag that has been added to the filters."
  :group 'tagging)

(defcustom tagging-debug nil
  "Debug general debug output for tagging"
  :type 'boolean
  :group 'tagging)

;;*var show debug custom jonathan
(defcustom tagging-debug-weight nil
   "Spit out relative weight after the tag"
   :type 'boolean
   :group 'tagging)

;;*var show debug custom jonathan
(defcustom tagging-debug-wrap nil
  "Show obscenely verbose debug info for wrapping"
  :type 'boolean 
  :group 'tagging)

;;*var custom show jonathan
(defcustom tagging-no-tagging-on-modes '(info-mode erc-mode)
   "A list of modes to disable tagging on."
   :type '(repeat symbol)
   :group 'tagging)

;;* var custom ecb jonathan
(defcustom tagging-buffer-sync 'basic
  "*Synchronize the tagging buffer automatically with the current buffer.

If 'always then the synchronization takes place always a buffer changes in the
edit window, if nil then never. If a list of major-modes then only if the
`major-mode' of the new buffer belongs NOT to this list.

If the special value 'basic is set then ECB uses the setting of the option
`ecb-basic-buffer-sync'.

IMPORTANT NOTE: Every time the synchronization is done the hook
`ecb-bufferinfo-buffer-sync-hook' is evaluated. "
  :type '(radio :tag "Synchronize ECBs example bufferino buffer"
				(const :tag "use basic value" :value basic)
				(const :tag "Always" :value always)
				(const :tag "Never" nil)
				(repeat :tag "Not with these modes"
						(symbol :tag "mode")))
  :group 'tagging)

;;* var custom ecb jonathan
(defcustom tagging-buffer-sync-delay 'basic
  "*Time Emacs must be idle before the bufferinfo-buffer is synchronized.
   Synchronizing is done with the current source displayed in the edit window. If
   nil then there is no delay, means synchronization takes place immediately. A
   small value of about 0.25 seconds saves CPU resources and you get even though
   almost the same effect as if you set no delay.

   If the special value 'basic is set then ECB uses the setting of the option
   `ecb-basic-buffer-sync-delay'"
  :group 'ecb-analyse
  :type '(radio (const :tag "use basic value" :value basic)
				(const :tag "No synchronizing delay" :value nil)
				(number :tag "Idle time before synchronizing" :value 2))
  :set (function (lambda (symbol value)
				   (set symbol value)
				   (if (and (boundp 'ecb-minor-mode)
							(featurep 'ecb-examples)
							ecb-minor-mode)
					   (ecb-activate-ecb-autocontrol-function
						value 'ecb-examples-bufferinfo-buffer-sync))))
  :initialize 'custom-initialize-default)

(defcustom tagging-buffer-sync-hook nil
  "Hook run at the end of `tagging-buffer-sync'.
   See documentation of `tagging-buffer-sync' for conditions when
   synchronization takes place and so in turn these hooks are evaluated."
  :group 'ecb-analyse
  :type 'hook)

;;*jonathan tagging-tags-hash show interactive
(defun tagging-show-tags ()
  "Shows all the tags in a buffer.

If ECB is activated, and you have an ECB window titled \"tagging\" then
this function will update that window."
  (interactive)
  (cond ((member major-mode tagging-no-tagging-on-modes)
		 ;(insert (propertize "Buffer mode ignored." 'face 'tagging-taglist-info-face))
		 )
		((null comment-start)
		 ;(insert (propertize "No Comment character defined." 'face 'tagging-taglist-info-face))
		 )
		(t
		 (tagging-scan-file)
		 (setq tagging-taglist-buffer (get-buffer-create tagging-taglist-buffer-name))
		 (tagging-edit-readonly (get-buffer-create tagging-taglist-buffer-name)
							   
								(goto-char (point-min))
								(erase-buffer)
								(insert (propertize (if (null tagging-filter-cur)
														"No filters applied\n"
														(format "Filter: %s\n" tagging-filter-cur))
													'face 'tagging-taglist-info-face))
								(let ((tagging-largest-tag-weight 0)) 
								  (maphash (lambda (k v) (when (> v tagging-largest-tag-weight)
															   (setq tagging-largest-tag-weight (float v))))
										   tagging-tags-hash)
								  (mapc 'tagging-show-tag
										(sort (hash-table-to-alist tagging-tags-hash)
											  'tagging-sort-car)))))))

;;*jonathan helper tagging-tags-hash 
(defun hash-table-to-alist (hash)
  "Build an alist from the values in HASH."
  (let ((list nil))
    (maphash (lambda (key value)
			   (setq list (cons (cons key value) list)))
			 hash)
    list))

;;*jonathan show helper
(defun tagging-sort-car (elt1 elt2)
  "Helper frunction for tagging-show-tags that will sort based on the car."
  (string-lessp (symbol-name (car elt1)) (symbol-name (car elt2))))

;;*jonathan show todo:fixme event
(defun tagging-show-tag (tag)
   "Shows the tag at a particular weight.

TAG is a cons, with the CAR being the tag symbol, and the CDR the weight.

This uses a totally evil non-local variable `tagging-largest-tag-weight'
for which I appologize profusely for using."
   (let ((scale (+ 0.25 (/ (cdr tag) tagging-largest-tag-weight)))
		 (face (cond ((member (cons (symbol-name (car tag)) t)
							  tagging-filter-cur)
					  'tagging-taglist-add-face)
					 ((member (cons (symbol-name (car tag)) nil)
							  tagging-filter-cur)
					  'tagging-taglist-sub-face)
			 		 (t 'tagging-taglist-face)))
		 (map (make-sparse-keymap)))
	 (define-key map [(control mouse-1)] 'tagging-mouse-event)
	 (define-key map [mouse-1] 'tagging-mouse-event)
	 (define-key map [mouse-2] 'tagging-mouse-event)
	 (define-key map [mouse-3] 'tagging-mouse-event)
	 (define-key map [enter] 'tagging-keyboard-add)
	 (define-key map [delete] 'tagging-keyboard-subtract)
	 (insert " " (propertize (symbol-name (car tag))
							 'face `(:inherit ,face
									 :height ,scale)
							 'pointer 'hand
							 'keymap map))
	 (when tagging-debug-wrap
		   (message "Pos: %s %s -- %s"
					(posn-x-y (posn-at-point (point) (get-buffer-window tagging-taglist-buffer)))
					(window-inside-pixel-edges (get-buffer-window tagging-taglist-buffer))
					(posn-at-point (point) (get-buffer-window tagging-taglist-buffer))))
	 (when (= (car (posn-x-y (posn-at-point (point)
											(get-buffer-window tagging-taglist-buffer))))
			  0)
		   (backward-char (length (symbol-name (car tag))))
		   (insert "\n")
		   (forward-char (length (symbol-name (car tag)))))
	 (when tagging-debug-weight
		   (insert ":" (format "%s" scale)))))

;;*jonathan show commands event filter ecb ecb-opt interactive
(defun tagging-mouse-event (event)
  "Handle a mouse event.  Typically not called directly by the user.

EVENT is passed in by emacs."
  (interactive "e")
  (when event
		(let ((tag-action (if (equal 'mouse-1 (car event))
							  'tagging-filter-add
							  'tagging-filter-subtract))
			  (tag-subtract (if (member 'control (event-modifiers event))
								"-"
								""))
			  (tag (save-excursion
					(select-window (posn-window (event-start event)))
					(goto-char (posn-point (event-start event)))
					(tagging-tag-at-point))))
		  (when tagging-debug (message "Captured event, %s tag %s %s" tag-action tag-subtract tag))
		  (if (featurep 'ecb)
			  (ecb-select-edit-window)
			  (other-buffer 't))
		  (funcall tag-action (concat tag-subtract tag))
		  (point-min))))


;;*jonathan ecb show
(defun tagging-ecb-sync-tagbuffer ()
  "Sync up the tags with ecb"
  (ecb-do-if-buffer-visible-in-ecb-frame
    'tagging-taglist-buffer-name
	(tagging-show-tags)))

;;*jonathan at-point 
(defun tagging-tag-at-point ()
  "Retrieve the current tag at the point"
  (save-excursion
   (let* ((match-start (re-search-backward (concat "[^" tagging-tag-characters "]")))
		  (match-end (progn (forward-char)
							(re-search-forward (concat "[^" tagging-tag-characters "]")))))
	 (if (and match-start match-end)
		 (buffer-substring-no-properties (+ 1 match-start) (- match-end 1))
		 nil))))

;;*jonathan ecb
(eval-after-load 'ecb
  (progn				 
   (defecb-autocontrol/sync-function tagging-buffer-sync
	 tagging-taglist-buffer-name tagging-buffer-sync t
	 "Synchronize the tag list buffer with the current source."
	 (tagging-ecb-sync-tagbuffer)
	 (run-hooks 'tagging-buffer-sync-hook))

   (defecb-window-dedicator ecb-set-tagging-buffer
	 tagging-taglist-buffer-name
	 (ecb-activate-ecb-autocontrol-function tagging-buffer-sync-delay 'tagging-buffer-sync)
	 (switch-to-buffer (get-buffer-create tagging-taglist-buffer-name))
	 (setq buffer-read-only t))
   nil))
  
  ;; (defecb-window-dedicator ecb-set-tagging-buffer
  ;; 	tagging-taglist-buffer
  ;; 	"Set the buffer in the current window to the bufferinfo-buffer and make this window dedicated for this buffer."
  ;; 	(switch-to-buffer (get-buffer-create
  ;; 					   tagging-taglist-buffer-name))
  ;; 	(setq buffer-read-only t))
  ;; (add-hook 'ecb-current-buffer-sync-hook 'tagging-ecb-sync-tagbuffer)
  

;;*search conrad tagging-filter-cur
(defun tagging-parse-tags (str) ;(tagging-parse-tags "foo -bar 340fdfv sd9fwe8dcmm _--3")
  (labels ((F (pos)
			  (let ((x (string-match (concat "-?[" tagging-tag-characters "]+") str pos)))
				(if x
					(cons (if (string= (substring str x (+ x 1)) "-")
							  (cons (substring str (+ x 1) (match-end 0)) nil)
							  (cons (substring str x (match-end 0)) t))
						  (F (match-end 0)))))))
		  (F 0)))

;;*search conrad
(defun tagging-tags-to-string (tags) ;(tagging-tags-to-string '((t . "foo") (nil . "bar")))
  (apply #'concat (mapcar (lambda (tag)
			    (concat (if (cdr tag)
					""
					"-")
				    (car tag)
				    " "))
			  tags)))

;;*jonathan scan tagging-tags-hash 
(defun tagging-scan-file ()
  "Scans the buffer for all tags.

This stores the all teh tags in `tagging-tags-hash'.
Return 'nil if it cannot scan."
  (interactive)
;; This isn't quite working up to snuff.
;;  (if (or (buffer-modified-p)
;;		  (= 0 (hash-table-count tagging-tags-hash))))
  (save-excursion
   (clrhash tagging-tags-hash)
   (goto-char (point-min))
   (while (re-search-forward tagging-tagline-indicator nil t)
		  (tagging-add-tags-to-hashmap (tagging-get-tags-on-line))
		  (forward-line 1))
   't))

;;*jonathan scan tagging-tags-hash
(defun tagging-get-tags-on-line ()
  "Search current line from point, and grab all tags."
  (let ((result '()))
	(while (re-search-forward (concat "\\([" tagging-tag-characters "]+\\)+") (line-end-position) t)
		   (let ((pos 1))
			 (while (match-string pos)
					(setq result (cons (match-string-no-properties pos) result))
					(setq pos (+ 1 pos)))))
	result))

;;*jonathan tagging-tags-hash
(defun tagging-add-tags-to-hashmap (tags)
  "Add a list of tags to the `tagging-tags-hash'."
  (when (not (null tags))
		(mapc (lambda (tag)
				(puthash tag
						 (+ 1 (gethash tag tagging-tags-hash 0))
						 tagging-tags-hash))
			  (mapcar 'intern tags))))

;;*search jonathan todo:fixme
(defun tagging-jump-to-tag (tag)
  "Jumps to TAG in the file."
  (interactive "sJump to tag: ")
   (string-match (concat "^" tagging-tagline-indicator ".*\b" tag "\b.*")))

;;*search conrad tagging-filter-cur jonathan todo:optimize
; Now that the fagging-filter-cur list is set as a proper alist, I bet this
; might be better optimized somehow.  
(defun tagging-search (tags)
  (let ((fails t))
    (while fails
		   (setq fails nil)
		   (if (re-search-forward tagging-tagline-indicator nil t)
			   (progn
				(beginning-of-line)
				(mapc (lambda (tag)
						(when (let ((x (re-search-forward (concat tagging-tagline-indicator
																  "\\(.* \\)?"
																  (car tag)
																  "\\( .*\\)?$")
														  (point-at-eol)
														  t)))
								(or (and (cdr tag) (not x)) (and (not (cdr tag)) x)))
							  (setq fails t))
						(beginning-of-line))
					  tags)
				(if fails
					(forward-line 1)))
			   (goto-char (point-max))))))

;;*search conrad overlay
(defun tagging-add-invisible-overlay (start end) 
  (let ((overlay (make-overlay start end))) 
    (setq tagging-invisible-areas (cons overlay tagging-invisible-areas)) 
    (overlay-put overlay 'invisible 'hl)))

;;*search conrad overlay
(defun tagging-make-visible () 
  (mapcar (lambda (overlay) (delete-overlay overlay))  
          tagging-invisible-areas) 
  (setq tagging-invisible-areas ()))

;;*search conrad tagging-filter-cur
(defun tagging-perform-filter (tags)
  (save-excursion
    (tagging-make-visible)
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (let ((x (point)))
	(tagging-search tags)
	(tagging-add-invisible-overlay x (point))
	(forward-line 1)
	(if (re-search-forward tagging-tagline-indicator nil t)
	    (beginning-of-line)
	    (goto-char (point-max)))))))

;;*conrad search
(defun tagging-show-all ()
  "Removes all filters on the sourceode.

See `tagging-filter-set' for more on filtering."
  (interactive)
  (tagging-make-visible)
  (setq tagging-filter-cur nil))

(defalias 'tagging-filter-none 'tagging-show-all)

;;*jonathan anything
(defvar anything-c-source-free-tagging
  '(((name . "Tags")
	 (init . (lambda () (setq anything-tags-buffer (tagging-tags-hash))))
	 (candidates . anything-c-free-tagging-candidates)
	 (action ("Jump to tag" . (lambda (c) (tagging-jump-to-tag c)))
	         ("Add to filter" . (lambda (c) (tagging-filter-add c)))
			 ("Subtract from filter" . (lambda (c) (tagging-filter-subtract c)))))))

;;*conrad search filter
(defun tagging-filter-set (tags)
  "Sets the tagging filter to TAGS.

The tagging filter will selectively display your sourcecode for you.
You can add or remove tags."
  (interactive (list (read-from-minibuffer "Tags: " (tagging-tags-to-string tagging-filter-cur))))
  (setq tagging-filter-cur (tagging-parse-tags tags))
  (tagging-perform-filter tagging-filter-cur)
  (message "Set tagging filter to %s" tagging-filter-cur))

;;*conrad search filter
(defun tagging-filter-add (tags)
  "Add TAGS to the filters.

See `tagging-filter-set' for more on filtering."
  (interactive (list (read-from-minibuffer "Tags: " (thing-at-point 'word))))
  (setq tagging-filter-cur (append tagging-filter-cur (tagging-parse-tags tags)))
  (tagging-perform-filter tagging-filter-cur)
  (message "Adding tag %s to filter %s" (tagging-parse-tags tags) tagging-filter-cur))

;;*conrad search filter
(defun tagging-filter-subtract (tags)
  "Remove TAGS from teh filters.

See `tagging-filter-set' for more on filtering."
  (interactive (list (read-from-minibuffer "Tags: " (thing-at-point 'word))))
  (mapc (lambda (x)
	  (setq tagging-filter-cur (delete x tagging-filter-cur)))
	(tagging-parse-tags tags))
  (tagging-perform-filter tagging-filter-cur)
  (message "Removing tag %s to filter %s" (tagging-parse-tags tags) tagging-filter-cur))

;;*conrad mode
(define-derived-mode tagging-mode fundamental-mode "Tagging"
    "Major mode for using del.icio.us- like tagging."
    (setq comment-start tagging-tagline-indicator)
    (local-set-key "\C-c\C-a" 'tagging-show-all)
    (local-set-key "\C-c\C-t" 'tagging-filter-set)
    (local-set-key "\C-c\C-s" 'tagging-filter-add)
    (local-set-key "\C-c\C-d" 'tagging-filter-subtract)
    (font-lock-mode)
     (setq font-lock-keywords (list `(,(concat tagging-tagline-indicator ".*$")
									  . font-lock-keyword-face))))

(add-to-list 'auto-mode-alist '(".tagged\\'" . tagging-mode))

;;*conrad mode
(define-minor-mode tagging-minor-mode
  "Toggle Tagging Minor mode."
  nil
  " Tagging"
  ;; The minor mode bindings.
  `(("\C-c\C-a" . tagging-show-all)
    ("\C-c\C-t" . tagging-filter-set)
    ("\C-c\C-s" . tagging-filter-add)
    ("\C-c\C-d" . tagging-filter-subtract)))

;;*jonathan mode-setup macro
(defmacro tagging-set-prefix (mode-hook tagline-prefix tag-characters)
  `(add-hook (quote ,mode-hook) (lambda ()
								  (set (make-local-variable 'tagging-tagline-indicator) ,tagline-prefix)
								  (set (make-local-variable 'tagging-tag-characters) ,tag-characters))))

(defmacro tagging-set-mode-options (package tagline-prefix tag-characters)
   (let ((mode (intern (concat (symbol-name package) "-mode")))
		 (hook (intern (concat (symbol-name package) "-mode-hook"))))
	`(when (featurep (quote ,package))
		   (tagging-set-prefix ,hook ,tagline-prefix ,tag-characters)
		   (quote ,package))))

;;*jonathan mode-setup
(defun tagging-setup-modes ()
  "This function is added to the emacs startup hooks to setup the regular expressions for tagging modes."
  (tagging-set-mode-options org "^\\s?\\*.*?\\s+:" "a-zA-Z0-9")
  (tagging-set-prefix java "^\\s*\\*\\s*@" tagging-tag-characters))

(add-hook 'emacs-startup-hook 'tagging-setup-modes)

;;*jonathan provide
(provide 'tagging)
(provide 'free-tagging)