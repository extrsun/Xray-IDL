pro get_acf_bts,radius,image_c,filter,length,angle,acf,acferr,acfelo,acfehi, $
countm,outfile=outfile,keepedge=keepedge,selv=selv
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -'
print,'get_acf_bts,radius,image_c,filter,length,angle,acf,acferr'
print,',acfelo,acfehi,countm,outfile=outfile,keepedge=keepedge,selv=selv'
return
endif
;
sz=size(image_c) 
;
dim_image=sz(1) < sz(2)
if sz(0) eq 3 then nsim=sz(3) else nsim=1
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
	nimage_c=fltarr(ndim,ndim,nsim)
;	nimage_c=dblarr(ndim,ndim)
	nfilter(edge:edge+dim_image-1,edge:edge+dim_image-1)=filter
	nimage_c(edge:edge+dim_image-1,edge:edge+dim_image-1,*)=image_c
endif else begin
;
; squeezing the dimension of the image
;
	edge=fix(dim_image-ndim)/2 ;biased to have a larger new image
	ndim=dim_image-2*edge
	nfilter=filter(edge:dim_image-1-edge,edge:dim_image-1-edge)
	nimage_c=float(image_c(edge:dim_image-1-edge,edge:dim_image-1-edge,*))
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
bin_sel=where(nfilter eq selv and dis le float(radius) and nimage_c(*,*,0) gt 0,nbin)
; In simulated images, some of these bins may contain no counts
;
if nbin eq 0  then begin
print,'No useful bin or count in the image'
return
endif 
;
sz=size(nimage_c)
rimage_c=reform(nimage_c,sz(1)*sz(2),nsim)
;
countm=total(rimage_c(bin_sel,*),1)/totnbin 	;mean count of the bins
;countrms=sqrt(total(nimage_c(bin_sel)*nimage_c(bin_sel))/totnbin-countm*countm)

if selv eq 1 then countbm=countm else $
	countbm=total(rimage_c(bin_selb,1))/totnbinb
rimage_c=rimage_c(bin_sel,*)

;the rms of the count distribution
print,'countm, countrms =',countm
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
acfup=fltarr(length+1,nsim)
totpair=acfup
;
; first sum up the multiplication of counts and mean number of counts at
; various distances 
;
cpair=fltarr(boxdim*boxdim,nsim)
for k=0L,(nbin-1) do begin

if !debug eq 2 then stop
     for n=0,nsim-1 do $
      cpair(*,n)=nimage_c(imin(k):imax(k),jmin(k):jmax(k),n)*rimage_c(k,n)
	good=incircle and  (nfilter(imin(k):imax(k),jmin(k):jmax(k)) GT 0.)
	get_posi,dis(where(good)),loci,kdup,nloci
	kdup=1./float(kdup)
	for n=1,(nloci-1) do begin 
	 ndis=loci(n)
	 pair=total(cpair(where(good and dis eq ndis),*),1)
	 totpair(ndis,*)=totpair(ndis,*)+pair
	 acfup(ndis,*)=acfup(ndis,*)+pair*kdup(n)
	endfor
endfor
;
; finally calculate the amplitudes and their uncertainties of the ACF 
; at the angles
;
if !debug eq 1 then stop
acf=fltarr(length,nsim)
den=totnbin*(countm*countbm)
for n=0,nsim-1 do acf(*,n)=acfup(1:length,n)/den(n)-1.
angle=indgen(length)+1 ;angles in units of pixels
;

;print,'length,countm,countrms,totnbin,countm,totnbin = '
;print,length,countm,countrms,totnbin,countbm,totnbinb

if nsim eq 1 then acferr=1./sqrt(totpair(1:length)*0.5 > 1.) else $
bts_conf,acf,acfm,acfelo,acfehi,siglevel=siglevel,/pri
;
if n_elements(outfile) ne 0 then  begin 
	openw,un,outfile,/get_lun
	printf,un,length,countm,totnbin
	for k=0,(length-1) do printf,un,angle(k),acf(k),acfelo(k),acfehi(k)
	free_lun,un
endif 
if !debug eq 1 then stop
end