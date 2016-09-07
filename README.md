# gpsd-lisp
A Common Lisp client for reading GPSD data.

GPSD is a daemon that sits between a physical GPS (or GLONASS) receiver and your software. It standardizes the format of GPS data so that no matter which proprietary (or semi-broken) standard your GPS receiver uses, you get a consistent usable output. It also allows multiple devices to share a single receiver, which is non-trivial when dealing with serial devices. This library brings the wonders of satellite navigation to your Common Lisp code. GPSD is available from (and documented at) http://catb.org/gpsd

The basic mode of operation of this library is to load the library, then kick off a thread that continuously monitors the GPSD daemon over a TCP connection, updating your location whenever new data is available.  The default is to connect to a GPSD server with the name "gpsd" on TCP port 2947.  If this does not match your setup, you will need to supply the appropriate parameters. Most people running GPSD simply run it on their local machine and bind only to localhost, or '127.0.0.1'.  To load the daemon and start the polling thread, do the following:

```
CL-USER> (ql:quickload :gpsd)
To load "gpsd":
  Load 1 ASDF system:
    gpsd
; Loading "gpsd"
.........
(:GPSD)
CL-USER> (gpsd:start-gpsd "127.0.0.1" 2947)
gpsd update thread is running...
NIL
CL-USER>
```

The officially supported method for getting your current location is to call (get-current-location), which returns an object containing your location (as well as a great deal of other data). There's a handy convenience function called (pp) that displays much of this data nicely:

```
CL-USER> (gpsd:pp (gpsd:get-current-location))
Time:  2016-09-07T13:24:18.000000-07:00
Lat:  41.237125
Lon:  -111.53106
Alt:  23.5
Spd:  0.306
Crs:  128.7265
Sats:  5
Mode:  3
Datum:  WGS84
NIL
CL-USER>
```

The fields in the object are:

* point-lat
* point-lon
* point-alt
* point-spd
* point-crs
* point-sats
* point-mode
* creation-time

Example:

```
CL-USER> (list (gpsd:point-lat (gpsd:get-current-location)) (gpsd:point-lon (gpsd:get-current-location)))
(41.23712541.237125 -111.53106)
CL-USER>
```

Most of these fields are self-explanatory. point-sats gives the number of satellites that were part of the current fix (more is better) and point-mode refers to the mode the receiver is in (documented on the GPSD web site). The short version is that a value of '2' means the receiver is in 2D mode (no valid altitude) and '3' indicates 3D mode (altitude is valid). creation-time is a unix time_t timestamp.

GPSD reports all values in metric. Speed is reported in meters/second and altitude is in meters. There are convenience macros in the GPSD library for converting to other units:

* m-to-ft
* ms-to-fs
* ms-to-fm
* ms-to-kph
* ms-to-kts
* ms-to-mph

Example (first example is altitude in meters, second is in feet):

```
CL-USER> (gpsd:point-alt (gpsd:get-current-location))
139.716
CL-USER> (gpsd:m-to-ft (gpsd:point-alt (gpsd:get-current-location)))
467.14236
CL-USER>
```

There is also a function to convert a decimal heading to a cardinal heading:

```
CL-USER> (gpsd:deg-to-cardinal-course (gpsd:point-crs (gpsd:get-current-location)))
"East"
CL-USER>
```

This library utilizes the location objects from aviation-formulary-lisp, which means it's also compatible with the various functions available in that library (distance between points, bearing between points, etc).
