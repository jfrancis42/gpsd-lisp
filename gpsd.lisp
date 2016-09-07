;;;; gpsd.lisp

(in-package #:gpsd)

(defparameter *gpsd-lock* (bt:make-lock))
(defparameter *uptodate-thread* nil)
(defvar *location* nil)

(defun socket-read (socket)
  (read-line (usocket:socket-stream socket)))

(defun socket-print (string socket)
  (write-line string (usocket:socket-stream socket))
  (force-output (usocket:socket-stream socket)))

(defun watch-gpsd (&optional (iterations 10) (host "gpsd") (port 2947))
  (let ((socket (usocket:socket-connect host port))
	(loc nil) (json nil) (sats nil))
    (socket-print "?WATCH={\"enable\":true,\"json\":true}" socket)
    (let ((result (json:decode-json-from-string (socket-read socket))))
      (if (equal "VERSION" (cdr (assoc :class result)))
	  (format t "Successful connection to GPSD v~A on ~A:~A~%"
		  (cdr (assoc :release result)) host port)))
    (loop for i from 1 to iterations do
	 (progn
	   (setf json (json:decode-json-from-string (socket-read socket)))
	   (cond
;	     ((equal "VERSION" (cdr (assoc :class json)))
;	      (print json))
;	     ((equal "DEVICES" (cdr (assoc :class json)))
;	      (print json))
	     ((equal "SKY"  (cdr (assoc :class json)))
	      (setf sats (length (remove-if-not (lambda (n) (cdr (assoc :used n))) (cdr (assoc :satellites json))))))
	     ((equal "TPV" (cdr (assoc :class json)))
	      (progn
		(setf loc (make-instance 'gps-point
					 :mode (cdr (assoc :mode json))
					 :sats sats
					 :lat (cdr (assoc :lat json))
					 :lon (cdr (assoc :lon json))
					 :alt (cdr (assoc :alt json))
					 :spd (cdr (assoc :speed json))
					 :crs (cdr (assoc :track json))))
		(format t "~%")
		(pp loc))))))
    (socket-print "?WATCH={\"enable\":false}" socket)
    (usocket:socket-close socket)
    ))

(defun up-to-dater (host port)
  (let ((socket (usocket:socket-connect host port))
	(loc nil) (json nil) (sats nil))
    (socket-print "?WATCH={\"enable\":true,\"json\":true}" socket)
    (loop
	 (progn
	   (setf json (json:decode-json-from-string (socket-read socket)))
	   (cond
	     ((equal "SKY"  (cdr (assoc :class json)))
	      (setf sats (length (remove-if-not (lambda (n) (cdr (assoc :used n))) (cdr (assoc :satellites json))))))
	     ((equal "TPV" (cdr (assoc :class json)))
	      (bt:with-lock-held (*gpsd-lock*)
		(setf *location* (make-instance 'gps-point
						:mode (cdr (assoc :mode json))
						:sats sats
						:lat (cdr (assoc :lat json))
						:lon (cdr (assoc :lon json))
						:alt (cdr (assoc :alt json))
						:spd (cdr (assoc :speed json))
						:crs (cdr (assoc :track json)))))))))))

(defun start-gpsd (&optional (host "gpsd") (port 2947))
  (setf *uptodate-thread* (bt:make-thread (lambda () (up-to-dater host port)) :name "gpsd"))
  (format t "gpsd update thread is running...~%"))

;; (bt:with-lock-held (gpsd:*gpsd-lock*) (gpsd:pp gpsd::*location*))
