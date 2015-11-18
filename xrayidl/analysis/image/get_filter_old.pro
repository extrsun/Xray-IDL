pro get_filter,filter,blow=blow,bhigh=bhigh,block=block,dim=dim $
,factor=factor,slow=slow,flow=flow,soufile=soufile,expfile=expfile,self=self
;+
; Get a filter with source areas set equal to 0
;*inputs:
; blow, bhigh - the lower and upper limits of the board bands (1-7)
; dim - the dimension of the image (shoudl be even value since the assumed
;   exposure map has a dimension of an even number of pixels
; factor - the radius factor of the source subtractions
; soufile - source file to be used for source subtractions
; slow - the minmum signal-to-noise ratio for a source to be included in
; the source subtraction
; flow - the minmum source count rate for a source to be included in
; the source subtraction
;*outputs: 
; filter - filter with area contaminated by sources subtracted.
; writen by wqd, May 11, 1994
;-
if N_params() eq 0 then begin
 print,'CALLING SEQUENCE - get_filter,filter,blow=blow,bhigh=bhigh,block=block'
 print,',dim=dim,factor=factor,slow=slow,flow=flow,soufile=soufile'
 print,',expfile=expfile'
 return
endif

if n_elements(slow) eq 0 then slow=0.
if n_elements(blow) eq 0 then blow=4
if n_elements(bhigh) eq 0 then bhigh=5
if n_elements(block) eq 0 then block=!block
if n_elements(dim) eq 0 then dim=512
if n_elements(factor) eq 0 then begin
if n_elements(soufile) eq 0 then soufile=!seq_no+'_sou_sass.dat'
factor=1.5
print,'factor = ',factor
endif
if n_elements(flow) eq 0 then begin
flow=0.0
print,'flow =',flow
endif
;
filter=fltarr(dim,dim)+1.
source_info,sid,sra,sdec,sigma,cntr,slow=slow,flow=flow,self=self,ns=ns,soufile=soufile
image_center,cra,cdec
print,'subtracting sources'
  filter=source_sub_v(filter,cra,cdec,sra,sdec,cntr, $
	factor=factor,blow=blow,bhigh=bhigh,block=block,subvalue=subvalue)
if keyword_set(expfile) ne 0 then begin
	hdr=headfits(expfile)
	livetime=sxpar(hdr,'LIVETIME')
	filter=filter*livetime
endif
;
end
