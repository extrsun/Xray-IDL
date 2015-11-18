;+
; NAME:
;     CT2LST2
; PURPOSE:
;     To convert local civil time to local mean sidereal time.
; CALLING SEQUENCE:
;     CT2LST2, Lst, Lng, Tz, Time, [Day, Mon, Year], reduced = reduced
; INPUTS:
;     Lng   The longitude in degrees of the place for which the local 
;           sidereal time is desired, scalar
;     Tz    The time zone of the site in hours.  Use this to easily account 
;           for Daylight Savings time (e.g. 4=EDT, 5 = EST/CDT), scalar
;     Tme   If the optional parameters are specified then this is the time
;           of day of the specified date in decimal hours.  If the optional
;           parameters are not specified then this is the Julian date of
;           time in question, scalar or vector
;     reduced  keyword parameter which, if Tme is the Julian date, tells
;              whether it is reduced Julian date (=1) or not (=0, default)
; OPTIONAL INPUTS:
;     Day   The day of the month.
;     Mon   The month, in numerical format.
;     Year  The year.
; OUTPUTS:
;     Lst   The Local Sideral Time for the date/time specified in hours.
; RESTRICTIONS:
;     If specified, the date should be in numerical form.  The year should
;     appear as yyyy.
; PROCEDURE:
;     The Julian date of the day and time is question is used to determine
;     the number of days to have passed since 0 Jan 1968.  This is used
;     in conjunction with the GST of that date to extrapolate to the current
;     GST; this is then used to get the LST.
; MODIFICATION HISTORY:
;     Adapted from the FORTRAN program GETSD by Michael R. Greason, STX, 
;          27 October 1988.
;     Accept
;     Adapted from ct2lst 3/4/92 (GAR)
;-
;                            If all parameters were given, then compute
;                            the Julian date; otherwise assume it is stored
;                            in Time.
;
PRO CT2LST2, lst, lng, tz, tme, day, mon, year, reduced = reduced
;
if n_params(0) eq 0 then begin
  print,' CT2LST2, lst, lng, tz, tme, day, mon, year, reduced = reduced'
  retall
endif
if (n_elements(reduced) eq 0) then reduced = 0
;
IF n_params(0) gt 4 THEN BEGIN
   time = tme + tz
   jdcnv2, year, mon, day, time, jd, red = reduced
ENDIF ELSE jd = double(tme)
sz = size(jd)
scalar = sz(0) eq 0
if scalar then jd = dblarr(1) + jd
;
;                            Useful constants.
;
jd1968 = 2439855.5D0
rjd1968 = 39855.5D0
c = [23696.535D0, 236.55536D0, 86636.55536D0]
;
;                            Compute GST in seconds.
;
if (reduced eq 0) then dd = double(jd) - jd1968  else $
                       dd = double(jd) - rjd1968
zmt = double(long(dd))
gst = c(0) + (c(1) * zmt) + (c(2) * (dd - zmt))
;
;                            Compute LST in hours.
;
lst = (gst / 3600.D0) - (double(lng) / 15.D0)
neg = where(lst lt 0.0D0, n)
if n gt 0 then lst(neg) = 24.D0 + (lst(neg) mod 24)
lst = lst mod 24.D0
if scalar then lst = lst(0)
;   
RETURN
END         ;pro ct2lst2
