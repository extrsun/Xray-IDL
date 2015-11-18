;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;NAME:
;timebin
;
;PURPOSE:
;construct binned light curve from events given in plist, in bins of
;equal (or mostly equal) length
;
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
;timebin,plist,tbin,rate,sigrate,nsec,tbeg=tbeg,tend=tend,binsize=binsize
;
;INPUTS:
;plist = structure of type xevent containing photon events (may have been 
;        spatially filtered (using pfilt), temporally filtered (using tfilt)
;        etc.
;tbeg = vector containing start times of intervals to be included
;tend =       "           stop        "                  "
;       optionally, tbeg can be [[tbeg],[tend]] (contain both start & stop
;       time info).
;binsize = width of time bin (in sec)
;
;OPTIONAL INPUT PARAMETERS:
;none
;
;OUTPUTS:
;tbin = midpoints of time bins
;rate = vector containing counting rate for each bin
;sigrate = errors on rate assuming Poisson statistics
;nsec = the duration of each time bin (in seconds)
;
;OPTIONAL OUTPUT PARAMETERS:
;none
;
;COMMON BLOCKS:
;none
;
;SIDE EFFECTS:
;none
;
;RESTRICTIONS:
;TBEG must be defined. TBEG and TEND can be the start & stop times of the
;good time intervals, if no time filtering was applied.
;If TEND is not defined, then TBEG must contain both start & stop time info.
;TBEG and TEND need not be contiguous. But data gaps within a given time 
;interval are NOT allowed.
;
;PROCEDURE:
;extracts list of events in list which lie within the boundaries of
;the time bins specified by binsize, tbeg, and tend
;Note that at present (2-19-91) procedure no exposure map correction 
;is applied.
;
;MODIFICATION HISTORY:
;  adapted from tbin (wrtten by M. Corcoran) 19 Feb 1992 (GAR)
;  modified 24 Oct 1993 (GAR) to use vsort & so to run faster
;
;-
;-------------------------------------------------------------------------------
pro timebin,plist,tbin,rate,sigrate,nsec,tbeg=tbeg,tend=tend,binsize=binsize
;
if n_params(0) eq 0 then begin
  print,' TIMEBIN, plist, TBIN, RATE, SIGRATE, nsec,'
  print,'          tbeg=tbeg, tend=tend, binsize=binsize'
  retall
end
;
sbeg = size(tbeg)
nbeg = n_elements(tbeg)
if (!debug gt 2) then stop,' Stopping in timebin at beginning.'
if (nbeg eq 0) then begin                     ;tbeg must be defined
  print,' TBEG must be defined. Returning.'
  retall
endif
nend = n_elements(tend)
if (nend eq 0) then begin                     ;tend not defined
  if (sbeg(0) ne 2) then begin
    print,' TEND not defined. TBEG must contain both start and stop times.'
    print,' Returning.'
    retall
  endif
  tstart = tbeg(*,0)
  tstop = tbeg(*,1)
endif else begin           ;tend is defined
  tstart = tbeg
  tstop = tend
endelse
nint = n_elements(tstart)
;
if (n_elements(binsize) eq 0) then read,' Bin size (seconds)? ',binsize
binsize = binsize*1.0
time = plist.time
;
; now we need to accumulate the photons, interval by interval
;
tbin = [-1.0D0] 
nsec = [-1.]
rate = tbin*0.
mult = 1./binsize
nsize = 10000.
if (!version.os eq 'vms') then nsize = 2*nsize
;
for ii=0,nint-1 do begin
  ind = 0
  ind = where( (time ge tstart(ii)) and (time le tstop(ii)),nsel )
  if (nsel gt 0) then begin        ;if there are photons in this interval
    tsel = time(ind) - tstart(ii)
;
; find numbers of photons at each arrival time within ii'th time interval
;
    if (nsel le nsize) then begin
      vsort,tsel,mult,truetimes,indsrt,jbeg,jend,nbi,mid=0
    endif else begin
      vsort_large,tsel,mult,truetimes,indsrt,jbeg,jend,nbi,$
                  mid=0,nsize=nsize
    endelse
;
    truetimes = truetimes + tstart(ii) 
    t1 = truetimes - binsize/2.
    t2 = truetimes + binsize/2.
    t2(nbi-1) = t2(nbi-1) < tstop(ii)
    tbin = [tbin,(t1+t2)/2.]
    nsec = [nsec,t2-t1]
    rate = [rate,jend - jbeg + 1]
    if (!debug gt 1) then $
       stop,' Stopping in intervals loop.'
  endif
endfor
tbin = tbin(1:*)                        ;strip off bogus first values
nsec = nsec(1:*)
rate = rate(1:*)
;
sigrate = sqrt(rate)/nsec               ;now divide by duration in each bin
rate = rate/nsec
;
return
end             ;pro timebin
