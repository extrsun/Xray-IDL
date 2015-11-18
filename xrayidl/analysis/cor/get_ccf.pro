pro get_ccf,radius,image_c,image_o,filter,length,angle,acf,acferr,nbin, $
countm,countrms,outfile=outfile,keepedge=keepedge,dir=dir,selv=selv
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -'
print,' get_acf,radius,image_c,filter,length,angle,acf,acferr,nbin'
print,' 		,countm,countrms,outfile=,keepedge=,dir=,selv=selv'
return
endif
;
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
	nimage_o=fltarr(ndim,ndim)
	nfilter(edge:edge+dim_image-1,edge:edge+dim_image-1)=filter
	nimage_o(edge:edge+dim_image-1,edge:edge+dim_image-1)=image_o
	nimage_c(edge:edge+dim_image-1,edge:edge+dim_image-1)=image_c
endif else begin
;
; squeezing the dimension of the image
;
	edge=fix(dim_image-ndim)/2 ;biased to have a larger new image
	ndim=dim_image-2*edge
	nfilter=filter(edge:dim_image-1-edge,edge:dim_image-1-edge)
	nimage_c=float(image_c(edge:dim_image-1-edge,edge:dim_image-1-edge))
	nimage_o=float(image_o(edge:dim_image-1-edge,edge:dim_image-1-edge))
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
	bin_selb=where(nfilter eq selv and dis le float(radius),totnbinb)

bin_sel=where(nfilter eq selv and dis le float(radius),totnbin)
;if atype le 3 then $ ;when the array type is integer (i.e., count image)
bin_sel=where(nfilter eq selv and dis le float(radius) and nimage_c ne 0,nbin)
;
if nbin eq 0  then begin
print,'No useful bin or count in the image'
return
endif 
;
totnbin=float(totnbin)
countm=total(nimage_c(bin_sel))/totnbin 	;mean count of the bins
countrms=sqrt(total(nimage_c(bin_sel)^2)/totnbin-countm^2)
countbm=total(nimage_o(bin_sel))/totnbin 

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
	cpair=nimage_o(imin(k):imax(k),jmin(k):jmax(k))*nimage_c(bin_sel(k))
	good=incircle and  (nfilter(imin(k):imax(k),jmin(k):jmax(k)) eq selv)
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
totpair=totpair*float(totnbin)/nbin ;to roughly account for pairs not included
					;in nbin
if !debug eq 1 then stop

	den=nbin*(countm*countbm)
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
end





