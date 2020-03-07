(import test chicken.time.posix
        (only chicken.io read-line)
        (only chicken.string conc)
        (only srfi-13 string-join)
        (only srfi-1 iota)
        chicken.process
        dt fmt)

(test-group
 "leap-year"
 
 (test #f (leap-year? 1999))
 (test #t (leap-year? 2000))
 (test #t (leap-year? 2004))
 (test #t (leap-year? 1404))
 (test #f (leap-year? 1400))  ;; <-- four years earlier but nope
 (test #t (leap-year? 0)))

(test-group
 "ymd->d"
 (test `(1 1) (receive (d->yd (ymd->d 1 1 1))))
 (test `(1000 1) (receive (d->yd (ymd->d 1000 1 #|jan|# 1))))
 (test `(2000 1) (receive (d->yd (ymd->d 2000 1 #|jan|# 1))))
 (test `(2000 1) (receive (d->yd (ymd->d 2000 1 #|jan|# 1))))
 
 (test `(2000 32) (receive (d->yd (ymd->d 2000 2 #|feb|# 1))))
 (test `(2000 42) (receive (d->yd (ymd->d 2000 2 #|feb|# 11))))

 (test `(2000 32) (receive (d->yd (ymd->d 2000 2 1))))
 
 (test (ymd->d 1 13 1) (ymd->d 2 1 1)))

(test-group
 "d->ymd"
 (test `(1 1 1) (receive (d->ymd 1)))
 (test `(1 12 31) (receive (d->ymd 365)))
 (test `(2 1 1) (receive (d->ymd 366)))
 (test `(1 3 1) (receive (d->ymd 60))))

(test-group
 "d->ywd"
 
 (test `(1 1 1) (receive (d->ywd 1)))
 (test `(2 1 1) (receive (d->ywd 365)))

 (test `(4 53 6) (receive (d->ywd 1462)))
 (test `(4 53 7) (receive (d->ywd 1463)))
 (test `(5 1 1) (receive (d->ywd 1464))))


;; "interesting" iso week numbers according to
;; https://en.wikipedia.org/wiki/ISO_week_date
(test-group
 "ywd where gregorian week number ≠ ISO week number (wikipedia)"
 (test `(2004 53 6) (receive (d->ywd (ymd->d 2005 01 01))))
 (test `(2004 53 7) (receive (d->ywd (ymd->d 2005 01 02))))
 (test `(2005 52 6) (receive (d->ywd (ymd->d 2005 12 31))))
 (test `(2005 52 7) (receive (d->ywd (ymd->d 2006 01 01))))
 (test `(2006 01 1) (receive (d->ywd (ymd->d 2006 01 02))))
 (test `(2006 52 7) (receive (d->ywd (ymd->d 2006 12 31))))
 (test `(2007 01 1) (receive (d->ywd (ymd->d 2007 01 01))))
 (test `(2007 52 7) (receive (d->ywd (ymd->d 2007 12 30))))
 (test `(2008 01 1) (receive (d->ywd (ymd->d 2007 12 31))))
 (test `(2008 01 2) (receive (d->ywd (ymd->d 2008 01 01))))
 (test `(2008 52 7) (receive (d->ywd (ymd->d 2008 12 28))))
 (test `(2009 01 1) (receive (d->ywd (ymd->d 2008 12 29))))
 (test `(2009 01 2) (receive (d->ywd (ymd->d 2008 12 30))))
 (test `(2009 01 3) (receive (d->ywd (ymd->d 2008 12 31))))
 (test `(2009 01 4) (receive (d->ywd (ymd->d 2009 01 01))))
 (test `(2009 53 4) (receive (d->ywd (ymd->d 2009 12 31))))
 (test `(2009 53 5) (receive (d->ywd (ymd->d 2010 01 01))))
 (test `(2009 53 6) (receive (d->ywd (ymd->d 2010 01 02))))
 (test `(2009 53 7) (receive (d->ywd (ymd->d 2010 01 03)))))

(define max-days 1095000)
(define fails
  (let loop ((d 1))
    (receive (yd doy) (d->yd d)
      (if (= 0 (modulo d 1693)) (print* "\rtesting rata die " (quotient (* 100 d) max-days) "%"
                                        " (year " yd ")...          "))
      (if (= d
             (yd->d yd doy)
             ((compose ywd->d d->ywd) d)
             ((compose ymd->d d->ymd) d))
          (if (< d max-days) (loop (+ d 1))
              0)
          1))))
(print* "\r")
(test (conc "testing d->...->d for days [1," max-days "]") 0 fails)

;; d is days since Rata Die 0.
(define (dtest d)
  (receive (y1 doy) (d->yd d)
    (receive (y2 woy dow) (d->ywd d)
      (receive (y3 m dom) (d->ymd d)
        (unless (= y1 y3) (error (conc "y2 ≠ y3: " y2 "≠" y3)))
        (let* ((cmd (conc "date -d '0001-01-01 + " d " days - 1 day'"
                          " '+((y %Y) (G %G) (m %m) (woy %V) (dow %u) (dom %d) (doy %j))'"))
               (expected (with-input-from-pipe cmd read))
               (actual
                `((y ,y1) (G ,y2)
                  (m ,m)
                  (woy ,woy)
                  (dow ,dow)
                  (dom ,dom)
                  (doy ,doy))))
          (when (= 0 (modulo d 100)) (print* "\rrd " d " " expected "..."))
          (if (equal? actual expected)
              (begin
                ;;(print "ok " expected "\nvs " actual)
                0)
              (begin
                (fmt #t nl "error on rata die day " d nl
                     "  expected: " (wrt expected) nl
                     "    actual: " (wrt actual) nl
                     "  running cmd: " (wrt cmd) nl nl)
                1)))))))
;;(dtest 1462)

(let loop ((d 1)
           (errors 0))
  (if  (> errors 10)
       (print "too many errors, stopping at rata die day " d)
       (loop (+ d 1) (+ errors (dtest d)))))


