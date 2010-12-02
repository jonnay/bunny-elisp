;;; neko-dbus.el --- An extension to dbus
(defconst neko-dbus-version "0.1")
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
;; neko-dbus is an extension of the emacs dbus libraries, to help
;; make writing extensions that plug into dbus much easier.
;;
;; This is more aimed at emacs hackers, rather then the casual user.
  
;;; Installation:
;; There should be much better installation notes then what is here.
;; but... there is not.  Sorry.  But at the very least, putting
;; this file on your load path and adding:
;(require 'neko-dbus)
;; should get you started.

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

(require 'dbus)

(defmacro def-neco-dbus-interface-call (name bus service path interface)
  "Helper macro to make repeated dbus-call-method calls easier.

The arguments are the same as the first 4 to `dbus-call-method'
This emits a documented defun which takes a METHOD argument, and any number of following arguments.

For instance, you might make a libpurple interface call like this:
 (def-bus-interface-call purple-call
   :session
   \"im.pidgin.purple.PurpleService\"
   \"/im/pidgin/purple/PurpleObject\"
   \"im.pidgin.purple.PurpleInterface\")

Then you could use it like this:
  (purle-call \"PurpleAccountsGetAllActive\")
  (purple-call \"PurpleConversationNew\" :int32 1 :int32 1023 \"someguy@someplace.com\")
"
  `(defun ,name (method &rest args)
	 (format "Calls the following dbus service METHOD with ARGS\n\nBus:%s\nService:%s\nPath:%s\nInterface:%s\n" ,bus ,service ,path ,interface)
     (apply 'dbus-call-method ,bus ,service ,path ,interface method args)))

(defun neko-dbus-inspect (service path)
  "Simple method to inspect a dbus object.  Mostly to help with debugging.

SERVICE is the name of the service to connect to
OBJECT is the object to inspect.
"
  (with-temp-buffer
   (insert (dbus-call-method :session service path "org.freedesktop.DBus.Introspectable" "Introspect"))
   (goto-char (point-min))
   (while (search-forward "\n" nil t) (replace-match "" nil t))
   (goto-char (point-min))
   (while (re-search-forward ">\s+< " nil t) (replace-match "><" nil t))
   (car (xml-parse-region (point-min) (point-max)))))

(defun neko-dbus-get-interfaces (service path)
  "Returns a list of available interfaces for SERVICE PATH"
  )

(provide 'neko-dbus)

;;; neko-dbus ends here
