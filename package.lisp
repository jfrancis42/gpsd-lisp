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
		:point-sats
		:point-mode
		:creation-time
		:creation-source
		:deg-to-cardinal-course
		:point-gps)
  (:export :watch-gpsd
	   :start-gpsd
	   :get-current-location
	   :pp
	   :*gpsd-lock*
	   :point-lat
	   :point-lon
	   :point-alt
	   :point-spd
	   :point-crs
	   :point-sats
	   :point-mode
	   :point-creation-time
	   :m-to-ft
	   :ms-to-fs
	   :ms-to-fm
	   :ms-to-kph
	   :ms-to-kts
	   :ms-to-mph
	   :deg-to-cardinal-course
	   :true-to-magnetic
	   :magnetic-to-true))
