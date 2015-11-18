;+
;
;NAME:
;timebin
;
;PURPOSE:
;construct binned light curve from events given in list, in bins of
;equal (or mostly equal) length
;
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
;timebin,list,tbin,rate,sigrate,nsec,tbeg=tbeg,tend=tend,binsize=binsize
;
;INPUTS:
;list = structure of type xevent containing photon events (may have been 
;       spatially filtered (using pfilt), temporally filtered (using tfilt)
;       etc.
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
;adapted from tbin (wrtten by M. Corcoran) 19 Feb 1992 (GAR)
;
;-
pro timebin,list,tbin,rate,sigrate,nsec,tbeg=tbeg,tend=tend,binsize=binsize
;
if n_params(0) eq 0 then begin
  print,' TIMEBIN, list, TBIN, RATE, SIGRATE, nsec,'
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
time = list.time 
;
; get the total number of time bins, the time midpoints, and the durations
;
tbin = [-1.0D0] & tbine = tbin & nsec = [-1.]
nbins = long((tstop - tstart)/binsize) + 1         ;no. bins per interval
ict = 0
for ii=0,nint-1 do begin
  nbi = nbins(ii)
  ict = ict + nbi           ;we are counting from a bogus first value
  tind = tstart(ii) + dindgen(nbi)*binsize
  tbin = [tbin,tind]                               ;start times of bins
  tbine = [tbine,tind+binsize]                     ;end times of bins
  nsec = [nsec,fltarr(nbi)+binsize]
  if (tbine(ict) gt tstop(ii)) then begin
    nsec(ict) = tstop(ii) - tbin(ict)              ;correct duration
    tbine(ict) = tstop(ii)
  endif
endfor
tbin = (tbin+tbine)/2.               ;calculate midpoints
tbine = 0
tbin = tbin(1:*)                     ;throw away bogus first values
nsec = nsec(1:*)
if (!debug gt 0) then stop,' Stopping in timebin after output bins defined.'
;
; now we need to accumulate the photons, interval by interval
;
rate = tbin*0.
ict = 0
for ii=0,nint-1 do begin
  nbi = nbins(ii)
;
; find all photons within ii'th time interval
;
  ind = 0
  ind = where( (time ge tstart(ii)) and (time le tstop(ii)) )
  if (ind(0) ge 0) then begin        ;if there are photons in this interval
    tsel = time(ind)
    mint = tstart(ii)
;
; find numbers of photons at each arrival time within ii'th time interval
;
    Loc = long( (tsel - mint)/binsize )   ;compute Location indices.
    Loc = [ Loc(sort(Loc)), -1 ]
    Ldup = where ( Loc(1:*)-Loc )         ;find duplicates.
    Loci = Loc(Ldup)			  ;unique Locations.
;
    if N_elements( Ldup ) EQ 1 then $
       Kdup = Ldup(0)+1   	$
       else Kdup = [ Ldup(0)+1 , Ldup(1:*)-Ldup ]  	; # duplicates at Loci.
;
    ind = 0
    ind = ict + indgen(nbi)
    rate(ind(loci)) = kdup
    if (!debug gt 1) then $
       stop,' Stopping in intervals loop.'
  endif
  ict = ict + nbi 
endfor
sigrate = sqrt(rate)/nsec               ;now divide by duration in each bin
rate = rate/nsec
;
return
end             ;pro timebin
