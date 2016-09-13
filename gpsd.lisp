;;;; gpsd.lisp

(in-package #:gpsd)

(defparameter *gpsd-lock* (bt:make-lock))
(defparameter *gpsd-thread* nil)
(defvar *location* nil)

;; meters to feet
(defmacro m-to-ft (m) `(* ,m 3.28084))

;; meters per second to feet per second
(defmacro ms-to-fs (m) `(* ,m 3.28084))

;; meters per second to feet per minute
(defmacro ms-to-fm (m) `(* ,m 196.85))

;; meters per second to kilometers per hour
(defmacro ms-to-kph (m) `(* ,m 3.6))

;; meters per second to miles per hour
(defmacro ms-to-mph (m) `(* ,m 2.23694))

;; meters per second to nautical miles per hour
(defmacro ms-to-kts (m) `(* ,m 1.94384))

(defun socket-read (socket)
  "Read a line of data from a socket (blocking)."
  (read-line (usocket:socket-stream socket)))

(defun socket-print (string socket)
  "Write data to a socket and flush."
  (write-line string (usocket:socket-stream socket))
  (force-output (usocket:socket-stream socket)))

(defun watch-gpsd (&optional (iterations 10) (host "gpsd") (port 2947))
  "Watch gpsd and display a new point every time the location is
updated for a specified number of iterations. Defaults to a gpsd
server named 'gpsd' and TCP port 2947. If you're running the gpsd on
the same machine this code is running on, the address should most
likely be '127.0.0.1'."
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
  "A thread that watches gpsd and keeps the internal data structures
updated."
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
						:creation-source point-gps
						:mode (cdr (assoc :mode json))
						:sats sats
						:lat (cdr (assoc :lat json))
						:lon (cdr (assoc :lon json))
						:alt (cdr (assoc :alt json))
						:spd (cdr (assoc :speed json))
						:crs (cdr (assoc :track json)))))))))))

(defun start-gpsd (&optional (host "127.0.0.1") (port 2947))
  "Start a thread that watches gpsd and constant updates the private
local variable *location*.  Defaults to a gpsd server named 'gpsd' and
TCP port 2947. If you're running the gpsd on the same machine this
code is running on, the address should most likely be
'127.0.0.1'."
  (setf *gpsd-thread* (bt:make-thread (lambda () (up-to-dater host port)) :name "gpsd"))
  (format t "gpsd update thread is running...~%"))

(defun get-current-location ()
  "The approved method of obtaining the current location."
  (if *gpsd-thread*
      (if (bt:thread-alive-p *gpsd-thread*)
	  (bt:with-lock-held (gpsd:*gpsd-lock*) gpsd::*location*)
	  nil)
      nil))
