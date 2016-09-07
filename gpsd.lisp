;;;; gpsd.lisp

(in-package #:gpsd)

;;; "gpsd" goes here. Hacks and glory await!

(defun socket-read (socket)
  (read-line (usocket:socket-stream socket)))

(defun socket-print (string socket)
  (write-line string (usocket:socket-stream socket))
  (force-output (usocket:socket-stream socket)))

(defun watch-gpsd ()
  (let ((socket (usocket:socket-connect "hoss.gritch.org" 2947))
	(loc nil) (json nil))
    (socket-print "?WATCH={\"enable\":true,\"json\":true}" socket)
    (loop for i from 1 to 100 do
	 (progn
	   (setf json (json:decode-json-from-string (socket-read socket)))
	   (cond
	     ((equal "VERSION" (cdr (assoc :class json)))
	      (print json))
	     ((equal "DEVICES" (cdr (assoc :class json)))
	      (print json))
	     ((equal "SKY"  (cdr (assoc :class json)))
	      (print json))
	     ((equal "SKY"  (cdr (assoc :class json)))
	      (print json))
	     ((equal "TPV" (cdr (assoc :class json)))
	      (progn
		(setf loc (make-instance 'af:gps-point
					 :lat (cdr (assoc :lat json))
					 :lon (cdr (assoc :lon json))
					 :alt (cdr (assoc :alt json))
					 :spd (cdr (assoc :speed json))
					 :crs (cdr (assoc :track json))))
		(pp loc))))))
    (socket-print "?WATCH={\"enable\":false}" socket)
    ))

(defun watch-gpsd2 ()
  (let ((socket (usocket:socket-connect "hoss.gritch.org" 2947))
	(loc nil) (json nil))
    (socket-print "?WATCH={\"enable\":true,\"json\":true}" socket)
    (loop
	 (progn
	   (setf json (json:decode-json-from-string (socket-read socket)))
	   (cond
	     ((equal "VERSION" (cdr (assoc :class json)))
	      (print json))
	     ((equal "DEVICES" (cdr (assoc :class json)))
	      (print json))
	     ((equal "SKY"  (cdr (assoc :class json)))
	      (print json))
	     ((equal "SKY"  (cdr (assoc :class json)))
	      (print json))
	     ((equal "TPV" (cdr (assoc :class json)))
	      (progn
		(setf loc (make-instance 'af:gps-point
					 :lat (cdr (assoc :lat json))
					 :lon (cdr (assoc :lon json))
					 :alt (cdr (assoc :alt json))
					 :spd (cdr (assoc :speed json))
					 :crs (cdr (assoc :track json))))
		(pp loc))))))))
