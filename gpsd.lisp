;;;; gpsd.lisp

(in-package #:gpsd)

(defconstant point-gps 2)

(defparameter *gpsd-lock* (bt:make-lock))
(defparameter *gpsd-thread* nil)

(defstruct loc time mode sats lat lon alt spd crs ss)
(defstruct other xdop ydop vdop tdop hdop gdop pdop)
(defstruct sat prn az el ss used)
(defparameter *location* (make-loc))
(defparameter *curr-sats* nil)

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

;; Decendant of 3d point.  Adds speed, course, and number of
;; satellites used information.
(defclass gps-point (af:3d-point)
  ((spd :accessor point-spd
	:initarg :spd
	:initform nil)
   (mode :accessor point-mode
	 :initarg :mode
	 :initform nil)
   (sats :accessor point-sats
	 :initarg :sats
	 :initform nil)
   (signal-strength :accessor point-signal-strength
		    :initarg :signal-strength
		    :initform nil)
   (crs :accessor point-crs
	:initarg :crs
	:initform nil)))

(defmethod point-serialize ((p gps-point))
  "Serialize a GPS point."
  (append
   (list
    '(type gps-point)
    (list 'lat (point-lat p))
    (list 'lon (point-lon p))
    (list 'datum (point-datum p))
    (list 'alt (point-alt p))
    (list 'spd (point-spd p))
    (list 'crs (point-crs p))
    (list 'sats (point-sats p))
    (list 'signal-strength (point-signal-strength p))
    (list 'mode (point-mode p)))
   (user-info-serialize p)))

(defmethod pp ((p gps-point))
  "Pretty print a GPS point."
  (format t "Name:  ~A~%" (point-name p))
  (format t "Descr:  ~A~%" (point-description p))
  (format t "Time:  ~A~%" (local-time:unix-to-timestamp (point-creation-time p)))
  (format t "Lat:  ~F~%" (point-lat p))
  (format t "Lon:  ~F~%" (point-lon p))
  (format t "Alt:  ~F~%" (point-alt p))
  (format t "Spd:  ~F~%" (point-spd p))
  (format t "Crs:  ~F~%" (point-crs p))
  (format t "Sats:  ~A~%" (point-sats p))
  (format t "SS:  ~A~%" (point-signal-strength p))
  (format t "Mode:  ~A~%" (point-mode p))
  (format t "Datum:  ~A~%" (point-datum p)))

(defmethod point-deserialize-method ((p gps-point) point-data)
  "Create an object from the data dumped by 'point-serialize'.  If the
optional point-type value is supplied, the created object will be of
that type."
  (user-info-deserialize-method p point-data)
  (mapcar #'(lambda (n)
	      (cond
	       ((equal (first n) 'lat)
		(setf (point-lat p) (second n)))
	       ((equal (first n) 'lon)
		(setf (point-lon p) (second n)))
	       ((equal (first n) 'spd)
		(setf (point-spd p) (second n)))
	       ((equal (first n) 'crs)
		(setf (point-crs p) (second n)))
	       ((equal (first n) 'alt)
		(setf (point-alt p) (second n)))
	       ((equal (first n) 'sats)
		(setf (point-sats p) (second n)))
	       ((equal (first n) 'signal-strength)
		(setf (point-signal-strength p) (second n)))
	       ((equal (first n) 'mode)
		(setf (point-mode p) (second n)))
	       ((equal (first n) 'datum)
		(setf (point-datum p) (second n)))
	       ))
	  point-data))

(defun up-to-dater (host port)
  "A thread that watches gpsd and keeps the internal data structures
updated."
  (let ((socket (usocket:socket-connect host port))
	(loc nil) (json nil))
    (socket-print "?WATCH={\"enable\":true,\"json\":true}" socket)
    (loop
	 (progn
	   (setf json (json:decode-json-from-string (socket-read socket)))
	   (cond
;;	     ((equal "VERSION" (cdr (assoc :class json)))
;;	      (print json))
;;	     ((equal "DEVICES" (cdr (assoc :class json)))
;;	      (print json))
	     ((equal "SKY"  (cdr (assoc :class json)))
	      (setf *curr-sats* (cdr (assoc :satellites json)))
	      (setf (loc-sats *location*) (length (remove-if-not (lambda (n) (cdr (assoc :used n))) (cdr (assoc :satellites json)))))
	      (setf (loc-ss  *location*) (* 1.0 (/ (apply '+ (mapcar (lambda (n) (if (cdr (assoc :used n)) (cdr (assoc :ss n)) 0)) (cdr (assoc :satellites json)))) (loc-sats *location*)))))
	     ((equal "TPV" (cdr (assoc :class json)))
	      (bt:with-lock-held (*gpsd-lock*)
		(setf (loc-time *location*) (cdr (assoc :time json)))
		(setf (loc-mode *location*) (cdr (assoc :mode json)))
		(setf (loc-lat *location*) (cdr (assoc :lat json)))
		(setf (loc-lon *location*) (cdr (assoc :lon json)))
		(setf (loc-alt *location*) (cdr (assoc :alt json)))
		(setf (loc-spd *location*) (cdr (assoc :speed json)))
		(setf (loc-crs *location*) (cdr (assoc :track json))))))))))
				     
(defun start-gpsd (&optional (host "127.0.0.1") (port 2947))
  "Start a thread that watches gpsd and constant updates the private
local variable *location*.  Defaults to a gpsd server named 'gpsd' and
TCP port 2947. If you're running the gpsd on the same machine this
code is running on, the address should most likely be
'127.0.0.1'."
  (setf *gpsd-thread* (bt:make-thread (lambda () (up-to-dater host port)) :name "gpsd"))
  (format t "gpsd update thread is running...~%"))

(defun get-current-sats ()
  "Get the current satellite info."
  *curr-sats*)

(defun get-current-location ()
  "The approved method of obtaining the current location."
  (if (bt:thread-alive-p *gpsd-thread*)
      (bt:with-lock-held (*gpsd-lock*)
	(make-instance 'gps-point
		       :creation-source point-gps
		       :mode (loc-mode *location*)
		       :sats (loc-sats *location*)
		       :signal-strength (loc-ss *location*)
		       :lat (loc-lat *location*)
		       :lon (loc-lon *location*)
		       :alt (loc-alt *location*)
		       :spd (loc-spd *location*)
		       :crs (loc-crs *location*)))
      nil))
