pro bts,seed,impos,outpos,nbs=nbs
;+
; NAME:
;	BTS
;
; PURPOSE:
;       Generate a bootstrap resampling of the data points
;
; CALLING SEQUENCE:
;       BTS,seed,impos,outpos,nbs=nbs
;
; INPUTS:
; seed - seed for the random number generator
; impos - positions of the imput data points 
;
; OPTIONAL INPUTS:
; nbs - number of bootstrapping samples (def=1)
; 
;	
; KEYWORD PARAMETERS:
;	KEY1:	Document keyword parameters like this. Note that the keyword
;		is shown in ALL CAPS!
;
;
; OUTPUTS:
; outpos - positions of the output data points
;
; SIDE EFFECTS:
;	Describe "side effects" here.  There aren't any?  Well, just delete
;	this entry.
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by:	WQD,  Feb 28, 1995
;-
if n_params() eq 0 then begin
print,'CALLING Sequence - bts,seed,impos,outpos,nbs=nbs'
return
endif
if n_elements(nbs) eq 0 then nbs=1
ns=n_elements(impos)
if ns*nbs gt 1.e7 then begin
print,'stop: The number of data points exceeds 10^7!!!'
print,'Type .c to continue.'
stop
endif
outpos=long(randomu(seed,ns,nbs)*ns)
outpos=impos(outpos)
return
end
