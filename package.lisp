;;;; package.lisp

(defpackage #:gpsd
  (:use #:cl)
  (:import-from :aviation-formulary
		:pp
		:gps-point
		:point-lat
		:point-lon
		:point-alt
		:point-spd
		:point-crs
		:creation-source
		:point-gps)
  (:export :watch-gpsd
	   :watch-gpsd2))
