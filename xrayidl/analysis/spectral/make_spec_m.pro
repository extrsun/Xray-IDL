;+
;
;NAME:
;make_spec
;
;PURPOSE:
;construct spectrum from events given in list, using arbitrary channel
;groupings (specified by group) 
;
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
; make_spec,list,etime,rate,sigrate,etime_eff
; ,pha=pha,group=group,gmin=gmin,gmax=gmax,syserr=syserr,hdr=hdr
;
;INPUTS:
;list = structure of type xevent containing photon events (may have been 
;       spatially filtered (using pfilt), temporally filtered (using tfilt)
;       etc.
;etime - the actural exposure time of the observation
;
;OPTIONAL INPUT PARAMETERS:
;none
;
;OUTPUTS:
;rate = vector containing counting rate for each one of 34 channels
;sigrate = errors on rate assuming Poisson statistics
;etime_eff = effective exposure time weighted by counts
;syserr = the systematic error given as the fraction of the counting rate
;         and is included in sigrate in quadranture
;
;OPTIONAL OUTPUT PARAMETERS:
; pha - =1 to use PHA channels, def=0 to use PI channels
; group - the group of channels, default is !group
; gmin - the lower group boundary of selected energy range
; gmax - the upper group boundary of selected energy range
;
;SUBROUTINES USED:
; off_ax_a
;
;COMMON BLOCKS:
;none
;
;SIDE EFFECTS:
;none
;
;RESTRICTIONS:
;none
;
;PROCEDURE:
;extracts PI spectral rates from a list of events within the PI channel 
;boundaries specified by !group. Vignetting corrections are calculated
; with the subroutine off_ax_a using analytic formula
;
;MODIFICATION HISTORY:
; A significant modified virsion of the original program writen by
; MC and GR. WQD, Nov 15, 1992.
; systematic error is included. WQD, Dec 2, 1992
;
;-
pro make_spec,list,etime,rate,sigrate,etime_eff $
,pha=pha,group=group,gmin=gmin,gmax=gmax,syserr=syserr,hdr=hdr
;
if n_params(0) eq 0 then begin
print,'CALLING SEQUENCE --- make_spec,list,etime,rate,sigrate,etime_eff'
print,',pha=pha,group=group,gmin=gmin,gmax=gmax,syserr=,hdr='
retall
endif
;
if n_elements(list) le 1 then begin ;list=-1 for no count in the list
	rate=0. & sigrate=0. & etime_eff=0.
	print,'the number of counts in the list <=1'
	return
endif
;
if (n_elements(group) eq 0) then group = !group     ;use !group as default
if n_elements(gmin) ne 0 then group=group(gmin:*,*)
if n_elements(gmax) ne 0 then group=group(0:gmax,*)
if (n_elements(pha) eq 0) then pha = 0              ;default is pi
if n_elements(syserr) eq 0 then syserr=0.03    ;systematic error 
;
s=size(group) & nchan=s(1)
rate=fltarr(nchan)
sigrate=rate
;	dcx=4096. ;from *.fits header, but apparently not right
;	dcy=4096.
	dcx=4119. ;from Steve's note
	dcy=3929.
	pixel_size=0.93420756 ;arcsec, or 14".947/16
;
xx=list.dx & yy=list.dy  ;to be corrected in the detector coordinate
;
; calculate the effective exposure time
;
tc=n_elements(list)
offax=sqrt((xx-dcx)*(xx-dcx)+(yy-dcy)*(yy-dcy) > 1.e-9) $
	*(pixel_size/60.)		;in units of arcminutes
off_ax_a_new,offax,list.pi,vig,ierr
if ierr eq 1 then stop,'ierr = 1 in off_ax_a_new' 
cntr=total(1./vig)/etime ; the total count rate in the entire energy range
print,'the count rate of the region is ',cntr
etime_eff=tc/cntr
print,'The effective exposure of the region is ',etime_eff
;
; now calculate the spectrum
;
if (pha eq 1) then begin
	stop,'the vignetting correction is not for PHA channels'
	print,'Accumulating PHA spectrum'
	spec=list.pha
endif else begin
	print,'Accumulating PI spectrum'
	spec=list.pi
endelse
;
for j=0,nchan-1 do begin
  indchan = where((spec ge group(j,0)) and (spec le group(j,1)),nc) 
  if (nc ne 0) then begin             ; if 1 or more counts
    rate(j) = total(1./vig(indchan))/etime
    sigrate(j) = rate(j)*sqrt(1./nc+syserr*syserr)
  endif else sigrate(j) = 1./etime_eff 
endfor
;
return
end             ;pro make_spec

