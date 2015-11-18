pro poisig,sig,expect,loc,image=image
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; poisig
;
;*PURPOSE:
; calculate the statistical significace of the number of counts in each 
; image pixel above an expected value at the same pixel, assuming a Poission
; distribution of the counts.
;
;*PARAMETERS:
;*INPUTS:
; expect - an array containing expected values (counts)
; loc - the locations of image pixels in the image.
; image - count image (long or integer)
;	if given, the locations will be calculated from the image.
;
;*OUTPUTS:
; sig - the significance (likelihood) for the image counts to be a randum
; 	realization of the expected value.
;
;*SUBROUTINES CALLED:
; remove
; get_posi
;*MODIFICATION HISTORY:
; writen Sept 3, 1996 (WQD)
;
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if N_params() eq 0 then begin
print,'CALLING SEQUENCE -  poisig,sig,expect,loc,image=image'
return
endif
;
if n_elements(image) ne 0 then image_loc,image,loc
get_posi,loc,loci,Kdup,nloci

if nloci eq 0 then begin
	print,'The image contains no counts'
	return
endif
maxe=max(expect)
if maxe eq 0. then begin
	print,'All expected value in the image equal to zero'
	return
endif

;
mc=max(kdup)
if mc gt 100 then stop,'max(expect) gt 100. Are you sure to proceed?'
;
expectd=double(expect)
prob=exp(-expectd)
sig=1.-prob

for n=1,mc do begin	
	prob(loci)=prob(loci)*expectd(loci)/double(n) 
	sig(loci)=sig(loci)-prob(loci)                
	sel=where(kdup eq n,nsel)
	if nsel ne 0 and n ne mc then remove,sel,loci,kdup
endfor 
;
return
end
