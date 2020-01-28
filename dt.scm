;;; inspired by (or one could say ported from)
;;; https://github.com/chansen/c-dt
(import test chicken.string)

;; ==================== ordinal dates ====================
(define (yd->d y d)
  (let ((y (if (< y 0)
               (error "TODO")
               (- y 1))))
    (+ (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       d)))

(define (d->yd d)
  (if (< d 1) (error "TODO"))

  (let ((d (- d 1)))
    (let ((y (* 400 (quotient d 146097)))
          (d (modulo d 146097)))
      (let* ((n100 (quotient d 36524))
             (y (+ y (* 100 n100))))
        (let ((d (modulo d 36524)))
          (let ((y (+ y (* 4 (quotient d 1461))))
                (d (modulo d 1461)))
            (let* ((n1 (quotient d 365))
                   (y (+ y n1))
                   (d (modulo d 365)))
              (if (or (= n100 4) (= n1 4))
                  (values (+ 0 y) 366)
                  (values (+ 1 y) (+ 1 d))))))))))

;; ==================== ymd ====================

;; (divides? 10 2)
;; (divides? 10 3)
(define (divides? n divisor)
  (= 0 (remainder n divisor)))

(define (leap-year? y)
  (cond ((divides? y 400) #t)
        ((divides? y 100) #f)
        ((divides? y 4)   #t)
        (else #f)))

;; (list (+ 31 29) (days-preceeding-month 3 2000))
;; (list (+ 31 28) (days-preceeding-month 3 2000))
(define (days-preceeding-month year month)
  (if (leap-year? year)
      (vector-ref #(0 0 31 60 91 121 152 182 213 244 274 305 335) month)
      (vector-ref #(0 0 31 59 90 120 151 181 212 243 273 304 334) month)))

(define (ymd->d y m d)
  (let* ((y (+ y (quotient m 12)))
         (m (modulo m 12)))
    (when (or (< m 1) (> m 12))
      (set! y (+ y (quotient m 12)))
      (set! m (modulo m 12))
      (when (< m 1)
        (set! y (- y 1))
        (set! m (+ m 12))))
    (yd->d y (+ d (days-preceeding-month y m)))))

(define (d->ymd d)
  (receive (y doy) (d->yd d)
    (let ((m (if (< doy 32)
                 1
                 ;; 1 + (5 * (doy - 59 - l) + 303) / 153
                 (+ 1 (quotient (+ (* 5 (- doy 59 (if (leap-year? y) 1 0))) 303) 153)))))
      (when (or (< m 1) (> m 12)) (error (conc "internal error: invalid m for d " d) m))
      (values y m (- doy (days-preceeding-month y m))))))

;; ==================== ywd ====================

(define (days-in-year y)
  (if (leap-year? y) 366 365))

(define (dow d)
  (let ((dow (modulo d 7)))
    (if (< dow 1) (+ dow 7) dow)))

(define (ywd->d y w d)
  (let ((dt (yd->d y 4)))
    (+ (- dt (dow dt)) (* 7 w) d -7)))

(define (d->ywd d)
  (receive (y doy) (d->yd d)
    (let* ((dow (dow d)))
      (set! doy (+ doy 4 (- dow)))
      (cond ((< doy 1)
             (set! y (- y 1))
             (set! doy (+ doy (days-in-year y))))
            ((> doy 365)
             (let ((diy (days-in-year y)))
               (when (> doy diy)
                 (set! doy (- doy diy))
                 (set! y (+ y 1))))))
      (values y (quotient (+ doy 6) 7) dow))))
