;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Input-output functions
;; Copyright (C) 2011, 2012  Michael J. Beer <michael.josef.beer@googlemail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; These functions are way less efficient than the other inefficient stuff
;; Mainly for discovering Common Lisp
;; Never to be used !

(in-package :common-lisp-user)

(defpackage :io
  (:use :common-lisp)
  (:export :read-vector
	   :read-vectors-from-list
	   :lispify
	   :output-table))

(in-package :io)

(defun get-token (&key stream type)
  "Returnes a token read from stream (STDIN if not given) and tries to convert it to TYPE (String if not given)"
  (labels ((get-token-as-list (read-something)
	     (let ((c (read-char stream nil)))
	       (cond ((or (not c) (eq c 'eof-value)) nil)
		     ((eq c #\ )
		      (cond (read-something nil)
			    (t (get-token-as-list t))))
		     ((eq c #\Newline)
		      (cond (read-something (unread-char c stream))
			    (t c)))
		     (t (let ((rest (get-token-as-list t)))
			  (cons c (cond ((eq rest #\Newline) nil)
					(t rest)))))))))
    (let ((token (get-token-as-list nil)))
      (cond ((not token) nil)
	    ((eq token #\Newline) nil)
	    (t (coerce token (cond ((eq type nil) 'string)
					     (t type))))))))


(defun read-vector (&optional in-stream (getter #'get-token))
  "Reads in several tokens from STREAM (STDIN if none given) and returns them as CONS list. Uses GETTER to retrieve the tokens (GET-TOKEN if none given)."
  (let ((token (funcall getter :stream in-stream)))
    (cond ((not token) nil)
	  (t (cons token (read-vector in-stream getter))))))


(defun read-vectors-from-list (&optional name)
  "Read in subsequent vectors from a file NAME untit an empty line is discovered."
  (labels ((read-vector-list (stream)
	     (let ((next (mapcar #'read-from-string (read-vector stream))))
	       (cond ((not next) nil)
		     (t (cons next (read-vector-list stream)))))))
    (cond (name
	   (let* ((instream (open name :direction :input))
		  (*read-eval* nil)
		  (vectors (read-vector-list instream)))
	     (close instream)
	     vectors))
	  (t (let ((*read-eval* nil))
	       (read-vector-list *standard-input*))))))


(defun lispify (vec)
  "Takes a list of strings, have all of them evaluated by clisp and return new list with the results"
  (let ((*read-eval* nil))
    (mapcar #'read-from-string vec)))



(defun output-table (table &optional (dest t))
  "Prints a table (a list containing lists of numbers) to stream DEST one row at a line"
  (labels ((output-line (line)
	     (cond ((not line) (format dest "~%"))
		   ((atom line) (format dest "~E~%" line))
		   (t (progn
			(format dest "~E" (car line))
			(output-line (cdr line)))))))
    (cond ((not table) nil)
	  (t (progn
	       (output-line (car table))
	       (output-table (cdr table)))))))


