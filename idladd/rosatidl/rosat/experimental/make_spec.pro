;+
;
;NAME:
;make_spec
;PURPOSE:
;construct spectrum from events given in list, using arbitrary channel
;groupings (specified by group)
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
;make_spec,plist,plinfo,rate,sigrate,pi=pi,group=group
;INPUTS:
;plist   structure of type xevent containing photon events (may have been 
;        spatially filtered (using pfilt), temporally filtered (using tfilt)
;        etc.
;plinfo  structure of type xevinfo which contains info concerning extraction
;        and observation (total exposure time, in particular)
;OPTIONAL INPUT PARAMETERS:
;none
;
;OUTPUTS:
;rate = vector containing counting rate for each one of 34 channels
;sigrate = errors on rate assuming Poisson statistics
;
;OPTIONAL OUTPUT PARAMETERS:
;none
;
;
;COMMON BLOCKS:
;none
;SIDE EFFECTS:
;none
;RESTRICTIONS:
;none
;
;PROCEDURE:
;extracts list of events in list which lie within the pha channel 
;boundaries specified by !group (must be created at startup, see
;~corcoran/idl/startup.pro) or by keyword group.
;
;MODIFICATION HISTORY:
;  adapted from mkspec (wrtten by M. Corcoran) 24 Jan 1992 (GAR)
;  modified 30 Oct 1992 (GAR) to remove correction for mean exposure map
;    This should be done a different way - applied as a multiplicative
;    correction to the spectral region as a whole, not photon by photon
;    As a result, blocking factor is no longer needed.
; modified 26 Jan 1993 (GAR) still need to divide rates and sigmas by
;    exposure time to get correct normalization in XSPEC! Get this info
;    from plinfo
; modified 27 Jan 1993 (GAR) to not use F Varosi's algorithm from imagxy
;    as I have been having problems with it
; modified 08 Feb 1993 (GAR) to fix bug when using groups for full (unbinned)
;    channel resolution
;-
pro make_spec,plist,plinfo,rate,sigrate,pha=pha,group=group
;
if n_params(0) eq 0 then begin
	print,' MAKE_SPEC, plist, plinfo, RATE, SIGRATE, pha=pha, group=group'
	retall
end
if (n_elements(group) eq 0) then group = !group     ;use !group as default
if (n_elements(pha) eq 0) then pha = 0              ;default is pi
exptime = plinfo.totexp
;
s=size(group) & nchan=s(1)
rate=fltarr(nchan)
sigrate=rate*0.
if (pha eq 1) then begin
	print,'Accumulating PHA spectrum. Not corrected for mean exposure.'
	spec=plist.pha
endif else begin
	print,'Accumulating PI spectrum. Not corrected for mean exposure.'
	spec=plist.pi
endelse
;
; find number of photons within each channel group
;
for j=0,nchan-1 do begin
  chandiff = group(j,1) - group(j,0)
  if (chandiff eq 0) then $
     isel = where(spec eq group(j,0),Kdup) else $
     isel = where( (spec ge group(j,0)) and (spec le group(j,1)),Kdup)
  rate(j) = Kdup
endfor
;
; uncertainty is given by square root(# photons in each group)
; afterwards, divide both rate and sigrate by exposure time
;
sigrate = sqrt(rate)
rate = rate/exptime
sigrate = sigrate/exptime
;
return
end             ;pro make_spec
