# Scheme ISO 8601 date library

This is basically a Scheme port of [Chansen's C
library](https://github.com/chansen/c-dt).

## Status

The API is incomplete and inconvenient, but the basic functionality
that's there has been tested against GNU's `date` and seems to produce
correct results.

## API

Both the API and this documentation needs work. The internal date
representation is `d`, a count of days since 1st of January year 0.

The following procedures are exported (`y` `m` `w` `d` is `year`
`month` `week` and `day` respectively):

```scheme
d->yd d->ymd d->ywd
yd->d ymd->d ywd->d
days-in-year ;;
leap-year? days-preceeding-month
```
