;+
;
;NAME:
;make_spec
;PURPOSE:
;construct spectrum from events given in list, using arbitrary channel
;groupings (specified by group) and blocking factors (specified by block)
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
;make_spec,list,mex,rate,sigrate,mtime,xmin,ymin,pha=pha,group=group
;	,groupmin=gmin,groupmax=gmax,block=block
;INPUTS:
;list = structure of type xevent containing photon events (may have been 
;       spatially filtered (using pfilt), temporally filtered (using tfilt)
;       etc.
;mex = instrument exposure map (as obtained by fits_get function)
;OPTIONAL INPUT PARAMETERS:
;none
;
;OUTPUTS:
;rate = vector containing counting rate for each one of 34 channels
;sigrate = errors on rate assuming Poisson statistics
;mtime = mean exposure time weighted by counts
;
;OPTIONAL OUTPUT PARAMETERS:
; pha - =1 to use PHA channels, def=0 to use PI channels
; group - the group of channels, default is !group
; groupmin - the lower group boundary of selected energy range
; groupmax - the upper group boundary of selected energy range
; block - the bin size of the exposure map (mex) in units of 0"5, def=30
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
;~corcoran/idl/startup.pro), and divides by exposure map to determine 
;exposure corrected rate.  Note that at present (12-9-91) procedure 
;uses the exposure map supplied by SASS, i.e. blocked by a factor of 30. 
;Such blocking is probably too coarse to due real spectroscopic analysis
;
;MODIFICATION HISTORY:
;adapted from mkspec (wrtten by M. Corcoran) 24 Jan 1992 (GAR)
;include the calculation of the mean exposure time mtime 11 Aug 1992 (WQD)
;excluding regions with zero exposure from the count rate calculations 
;14 Aug 1992(WQD)
;include parameters of groupmin and groupmax to trim the energy range of the 
;output spectra 28 Aug 1992 (WQD)
;
;-
pro make_spec,list,mex,rate,sigrate,mtime,xmin,ymin,pha=pha,group=group $
    ,groupmin=gmin,groupmax=gmax,block=block
;
if n_params(0) eq 0 then begin
	print,' make_spec,list,mex,RATE,SIGRATE,mtime,pha=pha,group=group,'
        print,' groupmin=gmin,groupmax=gmax,block=block'
	retall
endif
if (n_elements(group) eq 0) then group = !group     ;use !group as default
if n_elements(gmin) ne 0 then group=group(gmin:*,*)
if n_elements(gmax) ne 0 then group=group(0:gmax,*)
if (n_elements(block) eq 0) then block = !block     ;use !block as default
if (n_elements(pha) eq 0) then pha = 0              ;default is pi
if n_elements(xmin) eq 0 then xmin=0
if n_elements(ymin) eq 0 then ymin=0
;
s=size(group) & nchan=s(1)
rate=fltarr(nchan)
sigrate=rate*0.
if (pha eq 1) then begin
	print,'Accumulating PHA spectrum'
	spec=list.pha
endif else begin
	print,'Accumulating PI spectrum'
	spec=list.pi
endelse
;
xx=list.x & yy=list.y
s=size(mex) & nxmex=s(1)
;compute Location indices.
xx=xx-xmin
yy=yy-ymin
Loc = (xx*block)/30 + ((yy*block)/30) * nxmex 
;
; get the mean exposure time
;
; find number of photons at each photon position x and y. correct by value
; of exposure map at that position, and sum. 
; uncertainty is given by sum of squares of uncertainties fpr individual
; pixels (=sqrt(# photons in pixel)/value of exposure map at that pixel)
;
get_posi,loc,loci,kdup
c=where(mex(loci) gt 0.)
if c(0) ne -1 then begin
loci=loci(c)      ;only regions with non-zero exposure 
kdup=kdup(c)      ;only regions with non-zero exposure 
tc=total(kdup)
mtime=total(kdup*mex(loci))/tc
endif else stop,'the exposure in the region = 0'
print,'The count rate of the region is ',tc/mtime,' +/- ',sqrt(tc)/mtime
print,'The mean exposure of the region is ',mtime
;
for j=0,nchan-1 do begin
  indchan = where((spec gt group(j,0)) and (spec le group(j,1)))
  if (indchan(0) ne -1) then begin             ; if 1 or more counts
   get_posi,loc(indchan),loci,kdup
   c=where(mex(loci) gt 0.)
   if c(0) ne -1 then begin          ;will be accounted for
    loci=loci(c)      ;only regions with non-zero exposure 
    kdup=kdup(c)      ;only regions with non-zero exposure 
    rate(j) = total(imdiv(kdup,mex(loci)))
    sigrate(j) = sqrt( total(imdiv(kdup,mex(loci)*mex(loci)) ) )
   endif
  endif
endfor
;
if !debug eq 3 then stop,'stop at the end of make_spec'
return
end             ;pro make_spec

