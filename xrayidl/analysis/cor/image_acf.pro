pro image_acf,bandlow,bandhigh,angle,acf,acferr,area,image_c,image_t $
,block=block,dim=dim,tail=tail,factor=factor,plotoff=plotoff,inst=inst, $
length=length,tonly=tonly,soufile=soufile
;+
; Master program for calculating an ACF of an image
;*inputs:
; bandlow, bandhigh - the lower and upper limits of the board bands (1-7)
; dim - the dimension of the image (shoudl be even value since the assumed
;   exposure map has a dimension of an even number of pixels
; tail - the character(s) used for outfile
; factor - the radius factor of the source subtractions
; plotoff - if set, no plot
; inst - if set, the flat fielding acf will be calculated
; length - the maximum length of the acf will be calculated
;
;*outputs: 
; angle - the angles at which the ACF is calculated
; acf, acferr - the ACF and it uncertainty
; area - the total pixel numbers included in the calculation
; image_c,image_t - the count and exposure images used in the calculation
;-
if N_params() eq 0 then begin
print,'CALLING SEQUENCE - image_acf,bandlow,bandhigh,angle,acf,acferr,area,'
print,'image_c,image_t,block=block,dim=dim,tail=tail,factor=,plotoff=plotoff'
print,',soufile=soufile,inst=inst,length=,tonly'
return
endif
;
if n_elements(block) eq 0 then block=!block
if n_elements(dim) eq 0 then dim=140
chlow=[0,11,20,42,52,70,91,132]
chhigh=[0,19,41,51,69,90,131,201]
emin=chlow(blow)
emax=chhigh(bhigh)
print,'The energy region selected is emin = ',emin,'; emax = ',emax

bandtail=strtrim(blow,2)+strtrim(bhigh,2)
if n_elements(tail) eq 0 then tail=''
tail=tail+bandtail
outfile=!seq_no+'_acf'+tail+'.dat'
;
print,'get exposure map'
fname=!data_dir+!seq_no+'_exp'+bandtail+'.fits'
;fname=!data_dir+!seq_no+'_mex.fits'
image_t=readfits(fname,h)
crval=sxpar(h,'crval*')
crval=crval/!radeg
cra=crval(0)
cdec=crval(1)
;
 source_info,sid,sra,sdec,sigma,cntr,slow=slow,flow=flow $
	,ns=ns,soufile=soufile
;source_info,sid,sra,sdec,/self
;
if n_elements(factor) eq 0 then factor=1.
;image_t=rebin(image_t,(512*30)/block,(512*30)/block)
;image_t=image_cut(image_t,dim/2.,/pixel)
;image_t=source_sub(image_t,cra,cdec,sra,sdec,factor=factor,block=block)
  image_tsub=source_sub_v(image_t,cra,cdec,sra,sdec,cntr, $
	factor=factor,blow=blow,bhigh=bhigh,block=block,subvalue=subvalue)
; calculate  <v> and export image_t
if keyword_set(tonly) then begin
	time=sxpar(h,'XS-LIVTI')
	image_t=image_t/time
	c=where(image_t gt 0.,nc) 
	vmean=total(image_t(c))/nc
	print,'vmean = ',vmean
if !debug eq 1 then stop
	return
endif

if keyword_set(inst) ne 0 then image_c=1.e-4*image_t else begin
tmin=-1.
make_image,image_c,dim=dim,emin=emin,emax=emax,tmin=tmin,block=block
endelse
;
print,'now get the ACF'
;
if n_elements(length) eq 0 then length=10
get_acf,dim/2,image_c,image_t,length,angle,acf,acferr,area,outfile=outfile
;
if keyword_set(plotoff) eq 0 then begin
plot,angle,acf
errplot,angle,acf-acferr,acf+acferr
endif
if !debug eq 1 then stop
end