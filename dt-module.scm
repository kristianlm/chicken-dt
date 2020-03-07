(module dt (days-in-year
            d->yd d->ymd d->ywd
            yd->d ymd->d ywd->d
            leap-year? days-preceeding-month)
(import scheme  (chicken base))
(include "dt.scm"))
