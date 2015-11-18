;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       timefilter
;
;*PURPOSE:
;  Allow user to select data from a photon list structure for times within
;  a specified interval or intervals
;
;
;*CALLING SEQUENCE:
;       timefilter,plist,plinfo,fplist,fplinfo,tstart=tstart,tstop=tstop,$
;                  actfil=actfil
;
;*PARAMETERS:
; INPUTS:
;       plist  - Input photon list data structure (as created by MAKE_LIST)
;                Contains x & y, dx & dy positions,
;                arrival times, and pha & phi channels of photons.
;                has the structure of replicate(row,num), where
;                row={xevent,x:0,y:0,pha:0,pi:0,time:0.0D0,dx:0,dy:0}
;                and num is the total number of photons
;
;       plinfo - Data structure containing info concerning extraction of
;                original plist (as created by MAKE_LIST).
;                has the structure {xevinfo,obseq:'',filename:'',proc:'',
;                ctyp:'',xmin:0.0,xmax:0.0,ymin:0.0,ymax:0.0,
;                region:'',numpix:0.0D0,
;                totexp:0.0D0,ntimes:0,tbeg:dblarr(200),tend:dblarr(200)}
;
;         where  xmin,xmax,ymin & ymax are limits of box containing region,
;                region is the ASCII descriptor for the extraction region,
;                numpix = area of region (in pixels) of extraction region,
;                totexp = total time interval for extraction,
;                ntimes = number of time intervals, tbeg & tend are vectors
;                containing start & stop times of extraction intervals
;
; OPTIONAL INPUTS:
;
;       tstart - Interval start times for filter (may be single value or
;                vector) 
;       tstop  - Interval stop times for filter (may be single value or
;                vector) 
;       actfil - Name of file containing time intervals to be accepted
;                The file should contain one line per interval, with the
;                start and stop times. The last line should end in 2 zeroes.
;
;       NOTE: User must specify EITHER tstart and tstop OR actfil (not both).
;
; OUTPUTS:
;       fplist - Data structure for time filtered photon list
;       fplinfo  Information data structure for filtered photon list,
;                where plinfo.tbeg, plinfo.tend, and plinfo.totexp have
;                been changed to match the filter time intervals
;
;*PROCEDURE:
;   Uses the procedure TIMEINTSECT to determine the intersection between the
;   time intervals plinfo.tbeg,plinfo.tend and the time intervals given by
;   tstart,tstop (or read from ACTFIL). Determines which photons had arrival
;   times within these intervals.
;
;*EXAMPLES: 
;
;*RESTRICTIONS:
;   Time intervals must be specified using *either* TSTART & TSTOP *or*
;   ACTFIL; one of these options must be used.
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  TIMEINTSECT
;
;*MODIFICATION HISTORY:
;    written 15 Jul 93 (GAR)
;-
;-------------------------------------------------------------------------------
pro timefilter,plist,plinfo,fplist,fplinfo,tstart=tstart,tstop=tstop,$
               actfil=actfil
;
if (n_params(0) eq 0) then begin
  print,'TIMEFILTER,plist,plinfo,FPLIST,FPLINFO,tstart=tstart,tstop=tstop,'$
       +'actfil=actfil'
  print,'  '
  print,'   You must specify either TSTART & TSTOP or ACTFIL (but not both)'
  print,'  '
  retall
endif
;
if (n_elements(actfil) eq 0) then begin
  if ((n_elements(tstart) eq 0) or (n_elements(tstop) eq 0)) then begin
    print,' You must specify either TSTART & TSTOP or ACTFIL.'
    print,' Please check your inputs. Returning.'
    retall
  endif else begin
    if (n_elements(tstart) ne n_elements(tstop)) then begin
      print,' TSTART and TSTOP must have the same number of entries.'
      print,' Please check your inputs. Returning.'
      retall
    endif
    actbeg = double(tstart)       ;in case floating point specified
    actend = double(tstop)
  endelse
endif else begin
  if ((n_elements(tstart) ne 0) or (n_elements(tstop) ne 0)) then begin
    print,' You may not specify both TSTART & TSTOP and ACTFIL.'
    print,' Please check your inputs. Returning.'
    retall
  endif else begin                ;intervals will be read from a file
    openr,un,actfil,/get_lun    ;get free logical unit & open input ASCII file 
    actbeg = [-1.D0]
    actend = [-1.D0]
    tb = 0.D0
    te = 0.D0
    check = 1
    while ( (not eof(un)) and (check) ) do begin
      readf,un,tb,te
      check = (tb+te) gt 0
      if (check) then begin
        actbeg = [actbeg,tb]
        actend = [actend,te]
      endif
    endwhile
    actbeg = actbeg(1:*)
    actend = actend(1:*)
  endelse
endelse         ;defining actbeg,actend
;
; Now determine whether there are events which satisfy the time filter
;
nact = n_elements(actbeg)
try = plist.time
nsel = 0
iit = 0
while (iit lt nact) do begin
  tbot = actbeg(iit)
  ttop = actend(iit)
  indtry = where( (try ge tbot) and (try le ttop), nsel )
  if (nsel gt 0) then if (iit eq 0) then indt = indtry else $
     indt = [indt,indtry]
  iit = iit + 1
endwhile
;
; Now filter the list and update the info structure.
; If no events were found to satisfy the filter, return.
;
nsel = n_elements(indt)
if (nsel le 0) then begin
  print,' No events which match the selection criteria were found.'
  print,' Selection based on'
  print,'           tmin =', tmin,'   tmax =',tmax
  print,' Returning.'
endif else begin
  fplist = plist(indt)
;
; now get time intervals over which PLIST was extracted (from PLINFO)
; and find intersections with (ACTBEG,ACTEND)
;
  ntimes = plinfo.ntimes
  tbold = plinfo.tbeg(0:ntimes-1)
  teold = plinfo.tend(0:ntimes-1)
  tbeg = [-1.D0]
  tend = [-1.D0]
  for iit=0,nact-1 do begin    
    timeintsect,actbeg(iit),actend(iit),tbold,teold,trybeg,tryend,ntry
    tbeg = [tbeg,trybeg]
    tend = [tend,tryend]
  endfor
  tbeg = tbeg(1:*)
  tend = tend(1:*)
;
  fplinfo = plinfo
  fplinfo.totexp = total(tend - tbeg)
  ntimes = n_elements(tbeg)
  fplinfo.ntimes = ntimes
  ntimes = ntimes < 200
  fplinfo.tbeg = tbeg(0:ntimes - 1)
  fplinfo.tend = tend(0:ntimes - 1)
endelse
;
return
end          ;pro timefilter
