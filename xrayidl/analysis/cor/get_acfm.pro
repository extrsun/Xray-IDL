;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; get_acf
;
;*PURPOSE:
; Calculate an auto-correlation function (ACF) for an image
;
;*CALLING SEQUENCE:
; get_acf,radius,image_c,filter,length,angle,acf,acferr,nbin,countm,countrms
;	,outfile=outfile,keepedge=keepedge,dir=dir
;
;*PARAMETERS:
; INPUTS:
; radius - the radius within which the average <n(x)> is going to be made
; image_c - count image
; filter - exposure image normalized to 1, for instance, for selecting bins. 
; Only bins with values > 0 will be used in the 
;		calcuation of the ACF
; length - The maximum length (or angle) within which the ACF is going to be
; 		calculated. For a large value, the calculation could take
;		a long time.
; outfile - if ='no', no output file; if an other name is given, the outfile 
;         will be used as the output file name for the ACF
; keepedge - if set, the data between radius and radius+length will be 
;		partially used (see *PROCEDURE)
; dir - the directory to put the output file (def = !data_dir)
;
;*OUTPUTS:
; angle - containing the angles (in units of pixels) at which the ACF are 
;	calculated  (in units of pixels)
; acf - containing the ACF at the angles
; acferr - containing the corresponding standard uncertainties of the ACF 
; nbin - number bins within the radius with non-zero exposure 
; countm - the mean number of counts per bin within the radius
; countrms - the standard rms of the counts (or the second momentum)
; !seq_no_acf.dat (or outfile) - a file containing the following:
;			length countm countrms nbin
;			angle    acf   acferr
;
;*PROCEDURE:
; The definition of the ACF is <n(x)n(x+r)>/<n(x)>^2 - 1. 
; First, A box of the size of 2*length+1 is defined to be centered at each bin
; (with non-zero exposure) in the image. Then the count multiplications of 
; the bin with bins within the box are calculated, which is summed over
; the image (within radius+length if keepedge is set or within the radius only
; keepdge is not set). Finally the results are binned together according to 
; the angles of the bins in the box to the center.
;
;*EXAMPLES:
; get_acf,60,image_c,filter,10,angle,acf,acferr
; where image_c and filter may be images of dimension 160 X 160. The selection
; of the small value of the parameter radius avoids the use of the data near
; the edges of images
;
;*RESTRICTIONS:
;
;
;*NOTES:
; To avoid the effect of image boundaries, it is important to make the 
; calculations well within the boundaries.
;
;*SUBROUTINES CALLED:
; dist_circle
;
;
;*MODIFICATION HISTORY:
; writen Sept 6 1992 (WQD)
; include the key word keepedge Sept 14 (WQD)
;
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro get_acf,radius,image_c,filter,length,angle,acf,acferr,nbin, $
countm,countrms,outfile=outfile,keepedge=keepedge,dir=dir,selv=selv
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -'
print,' get_acf,radius,image_c,filter,length,angle,acf,acferr,nbin'
print,' 		,countm,countrms,outfile=,keepedge=,dir=,selv=selv'
return
endif
;
if n_elements(dir) eq 0 then dir=!data_dir
sz=size(image_c) 
;
dim_image=sz(1) < sz(2)
ndim=2*(radius+length) ;The estimated dimension of the image required
		  ;so that the box is always within the boundaries of the image
;
if dim_image lt ndim then begin
;
; expanding the dimension of the image to include an edge at the each side 
;
	edge=fix(ndim-dim_image)/2+1 ;biased to have a larger new image
	ndim=dim_image+2*edge
	nfilter=fltarr(ndim,ndim)
	nimage_c=fltarr(ndim,ndim)
;	nimage_c=dblarr(ndim,ndim)
	nfilter(edge:edge+dim_image-1,edge:edge+dim_image-1)=filter
	nimage_c(edge:edge+dim_image-1,edge:edge+dim_image-1)=image_c
endif else begin
;
; squeezing the dimension of the image
;
	edge=fix(dim_image-ndim)/2 ;biased to have a larger new image
	ndim=dim_image-2*edge
	nfilter=filter(edge:dim_image-1-edge,edge:dim_image-1-edge)
	nimage_c=float(image_c(edge:dim_image-1-edge,edge:dim_image-1-edge))
endelse
;
; find only those bins within the radius
;
dist_circle,dis,ndim,(ndim-1.)/2.,(ndim-1.)/2.
;
if keyword_set(keepedge) eq 0 then begin 
print,'Bins outside the radius will not be used entirely '
	nfilter(where(dis gt float(radius)))=0.
endif

if n_elements(selv) eq 0 then begin
	selv=1. 
	totnbinb=0
endif else $
	bin_selb=where(nfilter gt 0. and dis le float(radius),totnbinb)

bin_sel=where(nfilter eq selv and dis le float(radius),totnbin)
bin_sel=where(nfilter eq selv and dis le float(radius) and nimage_c gt 0,nbin)
;
if nbin eq 0  then begin
print,'No useful bin or count in the image'
return
endif 
;
countm=total(nimage_c(bin_sel))/totnbin 	;mean count of the bins
countrms=sqrt(total(nimage_c(bin_sel)*nimage_c(bin_sel))/totnbin-countm*countm)
if selv eq 1 then countbm=countm else $
	countbm=total(nimage_c(bin_selb))/totnbinb

;the rms of the count distribution
print,'countm, countrms =',countm,countrms
;
j=bin_sel/long(ndim) & i=bin_sel MOD long(ndim) ;the i,j positions of the bins
;
;calculate the min and max of the i,j for the box
;
jmin=(j-length) 
jmax=(j+length) 
imin=(i-length) 
imax=(i+length) 
;
; define the box (or a square array) 
;
boxdim=2*length+1              ;dimension of the box
;
; obtain the distances of the bins from the center of the box
;
dist_circle,dis,boxdim,float(length),float(length)
dis=nint(dis)             ;where the bins will belong to
incircle=dis le length ;set 1 for those bins within the length
;
acf=fltarr(length+1)
totpair=acf
;
; first sum up the multiplication of counts and mean number of counts at
; various distances 
;
for k=0L,(nbin-1) do begin

if !debug eq 2 then stop
	cpair=nimage_c(imin(k):imax(k),jmin(k):jmax(k))*nimage_c(bin_sel(k))
	good=incircle and  (nfilter(imin(k):imax(k),jmin(k):jmax(k)) GT 0.)
	get_posi,dis(where(good)),loci,kdup,nloci
	kdup=1./float(kdup)
	for n=1,(nloci-1) do begin 
	 ndis=loci(n)
	 pair=total(cpair(where(good and dis eq ndis)))
	 totpair(ndis)=totpair(ndis)+pair
	 acf(ndis)=acf(ndis)+pair*kdup(n)
	endfor
endfor
;
; finally calculate the amplitudes and their uncertainties of the ACF 
; at the angles
;
if !debug eq 1 then stop

	den=totnbin*(countm*countbm)
	acferr=1./sqrt(totpair(1:length)*0.5 > 1.)
	acf=acf(1:length)/den-1.
	angle=indgen(length)+1 ;angles in units of pixels

;
print,'length,countm,countrms,totnbin,countm,totnbin = '
print,length,countm,countrms,totnbin,countbm,totnbinb
for k=0,(length-1) do begin
print,angle(k),acf(k),acferr(k)
endfor
;
if n_elements(outfile) eq 0 then outfile=dir+!seq_no+'_acf.dat' $
else begin 
	if outfile eq 'no' or outfile eq 'NO' then goto,finish $
	else outfile=dir+outfile
endelse
print,'output into the file ',outfile
openw,un,outfile,/get_lun
printf,un,length,countm,countrms,totnbin
for k=0,(length-1) do begin
printf,un,angle(k),acf(k),acferr(k)
endfor
free_lun,un
finish:
;
nbin=totnbin
if !debug eq 1 then stop
end