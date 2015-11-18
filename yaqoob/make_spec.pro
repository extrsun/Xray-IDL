;+
;
;NAME:
;make_spec
;PURPOSE:
;construct spectrum from events given in list
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
;make_spec,list,plinfo,cnts,sigcnts,pha=pha
;INPUTS:
;list   structure of type xevent containing photon events (may have been 
;        spatially filtered (using pfilt), temporally filtered (using tfilt)
;        etc.
;plinfo  structure of type xevinfo which contains info concerning extraction
;        and observation (total exposure time, in particular)
;OPTIONAL INPUT PARAMETERS:
;none
;
;OUTPUTS:
;cnts = vector containing counts for each channel
;sigcnts = errors on cnts assuming Poisson statistics
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
; modified 09 Feb by PJS to give cnts rather than a rate
;-
pro make_spec,list,plinfo,cnts,sigcnts,pha=pha
;
if n_params(0) eq 0 then begin
	print,' MAKE_SPEC, list, plinfo, CNTS, SIGCNTS,pha=pha
	retall
end
group=rebin((indgen(256)+1),256,2)      
if (n_elements(pha) eq 0) then pha = 0              ;default is pi
exptime = plinfo.totexp
;
s=size(group) & nchan=s(1)
cnts=fltarr(nchan)
sigcnts=cnts*0.
if (pha eq 1) then begin
	print,'Accumulating PHA spectrum. Not corrected for mean exposure.'
	spec=list.pha
endif else begin
	print,'Accumulating PI spectrum. Not corrected for mean exposure.'
	spec=list.pi
endelse
;
; find number of photons within each channel group
;
for j=0,nchan-1 do begin
  chandiff = group(j,1) - group(j,0)
  if (chandiff eq 0) then $
     isel = where(spec eq group(j,0),Kdup) else $
     isel = where( (spec ge group(j,0)) and (spec lt group(j,1)),Kdup)
  cnts(j) = Kdup
endfor
;
; uncertainty is given by square root(# photons in each group)
; afterwards, divide both rate and sigrate by exposure time
;
sigcnts = sqrt(cnts)
;
return
end             ;pro make_spec
