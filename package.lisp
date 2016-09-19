;;;; package.lisp

(defpackage #:gpsd
  (:use #:cl)
  (:import-from :aviation-formulary
		:pp
		:point-serial-number
		:point-creation-time
		:point-creation-source
		:point-name
		:point-description
		:point-lat
		:point-lon
		:point-datum
		:point-alt
		:deg-to-cardinal-course)
  (:export :watch-gpsd
	   :start-gpsd
	   :get-current-location
	   :pp
	   :*gpsd-lock*
	   :gps-point
	   :point-serial-number
	   :point-creation-time
	   :point-creation-source
	   :point-name
	   :point-description
	   :point-lat
	   :point-lon
	   :point-datum
	   :point-alt
	   :point-sats
	   :point-signal-strength
	   :point-mode
	   :point-spd
	   :point-crs
	   :m-to-ft
	   :ms-to-fs
	   :ms-to-fm
	   :ms-to-kph
	   :ms-to-kts
	   :ms-to-mph
	   :deg-to-cardinal-course
	   :true-to-magnetic
	   :magnetic-to-true))
