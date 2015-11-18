pro gal_dif_im,im,hdr,imout,filter=filter,imin=imin,imax=imax,ncol=ncol,glam,glalo,glahi,colval,colvallo,colvalhi,modv
;-
; calculate a model distribution of a Galactic diffuse emission based on 
; an input emission image and 2-D disk model for emission
;
; INputs:
; im -  image 
; hdr - the fits header of the image (use get_fitshead to create a minimum 
;	fits header)
; imin,imax - the minimum and maxmum values of the image bins to be 
; 		included in the fit (def=0,10^20)
; filter - pixels with filter value < 0. will not be used in the model fit
;*OUTPUTS:
; imout - the model image 
;
; writen by WQD, 4/17/94
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - gal_dif_im,glad,im,imout,filter=filter'
print,',imin=imin,imax=imax,ncol=ncol,glam,glalo,glahi,colval,colvallo,colvalhi,modv'
return
endif

if n_elements(imin) eq 0 then imin=0.
if n_elements(imax) eq 0 then imax=1.e20 ;essentially infinite 
if n_elements(filter) eq 0 then filter=im*0.+1.

sel=where(im ge imin and im le imax and filter gt 0.,nsel)
if nsel eq 0 then stop,'there is no bin selected'
binvals=fltarr(nsel)
binvals(0:*)=im(sel)
glactc_im,im,hdr,glad ;get Galactic latitudes for individual pixels
glads=glad(sel)

; get the median values for individual latitude intervals
get_ref_median,glads,binvals,colval,colvallo,colvalhi,glam,glalo,glahi $
	,ncol=ncol,nbin=nbin

; fit the median values with a 2-D disk model
galactc_fit_v,colval,glam,coef ;least square fit

		;,ww=(nbin-1.)/(colval-colvallo)/(colvalhi-colval)

; plot the data and model
modv=coef(0)+coef(1)/sin(abs(glam*!pi/180.))
plot_xy,glam,glalo,glahi,colval,colvallo,colvalhi,modv,psym=psym,xt=xt,yt=yt
stop,'You can get a hardcopy here by using plot_xy'
;
; get the model image of the diffuse distribution
imout=im*0.
imout(*)=coef(0)+coef(1)/sin(abs(glad*!pi/180.))
end
