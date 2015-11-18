pro read_mimages,nbo,hdr,fs,sfs,mv,noplot=noplot,cor=cor,edge=edge,mdim=mdim,xshift=xshift,yshift=yshift,bnorm=bnorm,filter=filter,bfoffset=bfoffset
;+
; read mcsmooth.e outputs
; nb - number of 2-d images
; hdr - fits header of the 2-d images
; fs - output array containing the images
; sfs - output alog10 scaled images
; mv - median values of the images 
; noplot - if set, no plot 
; cor - output boundaries of the plot for overploting
; edge - x and y number of pixels to be cut out from each side of the images
; 	
; mdim - output dimensions of the 2-d images (e.g., [512,512])
; xshift, yshift - shift of image center in x (right) and y (up) direction, 
;	which should be smaller than edge
; bnorm - normalization factor of the background images to be subtracted
; 	necessary if the background subtraction is wanted. (files b_?.fits 
; 	must be available)
; filter - image (e.g., exposure map) used to filter out regions with
;          image values < = 0 (e.g., unexposed regions)
; bfoffset - if set, the median backg values are added back into 
;        the background subtracted intensity in each band
;
;*example:
; read_mimages,4,mh,fs,a,mv,cor=cor,edge=6,mdim=mdim
; source_plot,soufile,cor,mh,/fits,probth=probth,psym=6,sym=2
;
; written by wqd, July 4, 2002
; add the keyword filter, wqd, 5/7/2003
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - read_mimages,nbo,hdr,fs,sfs,mv,noplot=noplot,cor=cor,edge=edge,mdim=mdim,xshift=xshift,yshift=yshift,bnorm=bnorm,filter=filter,bfoffset=bfoffset'
return
endif
if n_elements(nbo) eq 1 then begin
    kk=indgen(nbo)
    nb=nbo
endif else begin
    kk=nbo
    nb=n_elements(nbo)
endelse
if n_elements(filter) ne 0 then begin
    sel=where(filter le 0.,nsel)
    ksel=where(filter gt 0.,nksel)
    endif else nsel=0
;hdr=headfits('t_1_s.fits')
;pixsize=sxpar(hdr,'cdelt1') 
;norm=1.e3/(pixsize*60.)^2 
;mdim=[sxpar(hdr,'naxis1'),sxpar(hdr,'naxis2')]
;fs=fltarr(mdim(0),mdim(1),nb)
for n=0,nb-1 do begin
    k=kk(n)
	sk=strtrim(k+1,2)
        if n eq 0 then begin
            tt=readfits('t_'+sk+'_s.fits',hdr)
            pixsize=sxpar(hdr,'cdelt1')
            norm=1.e3/(pixsize*60.)^2 
            mdim=[sxpar(hdr,'naxis1'),sxpar(hdr,'naxis2')]
            fs=fltarr(mdim(0),mdim(1),nb)
        endif else tt=readfits('t_'+sk+'_s.fits')
        
        if nsel ne 0 then tt(sel)=0.
        fs(*,*,n)=imdiv(readfits('c_'+sk+'_s.fits'),tt)
	if n_elements(bnorm) ne 0 then begin
            bb=imdiv(bnorm*readfits('b_'+sk+'_s.fits'),tt)
            fs(*,*,n)=fs(*,*,n)- bb
            if keyword_set(bfoffset) then begin
                msel=where(tt gt 0)
                bmv=bb*0.
                bmv(msel)=median(bb(msel))
                fs(*,*,n)=fs(*,*,n)+bmv
            endif 
	endif 
    endfor
nedge=n_elements(edge)
case nedge of
 0: edge=[[1,1]*0,mdim(0)-1,mdim(1)-1]
 1: edge=[[1,1]*edge,mdim(0)-edge-1,mdim(1)-edge-1]
 2: edge=[edge(0),edge(1),mdim(0)-edge(0)-1,mdim(1)-edge(1)-1]
 else:
endcase
if n_elements(xshift) eq 0 then xshift=0
if n_elements(yshift) eq 0 then yshift=0

if nedge ne 0 then begin
	mdim=[edge(2)-edge(0)+1,edge(3)-edge(1)+1]
	temp=fs
	thdr=hdr
	fs=fltarr(mdim(0),mdim(1),nb)
	for k=0,nb-1 do $
		fs(*,*,k)=temp(xshift+edge(0):xshift+edge(2), $
			yshift+edge(1):yshift+edge(3),k)
	crpix=sxpar(hdr,'crpix*')
	get_fitshead,fltarr(mdim(0),mdim(1)),hdr,thdr $
		,cpx=crpix(0)-edge(0)-xshift,cpy=crpix(1)-edge(1)-yshift
                                ;assuming that the reference pixel is
                                ;at the center of the image
        if nsel ne 0 then begin
            temp=filter(xshift+edge(0):xshift+edge(2), $
			yshift+edge(1):yshift+edge(3))
            ksel=where(temp gt 0.,nksel) 
        endif 
endif

fs=fs*norm ;in units of 1.e3 cts/s/arcmin^2
mv=fltarr(nb)
for k=0,nb-1 do begin
	if nsel ne 0 then begin
            temp=fs(*,*,k)
            if n_elements(ksel) ne 0 then temp=temp(ksel)
            mv(k)=median(temp)
        endif else mv(k)=median(fs(*,*,k))
        print,'k,mv(k) = ',k,mv(k)
endfor
; scaled images:
sfs=fltarr(mdim(0),mdim(1),3)
if nb eq 3 then sfs(*,*,0)=bscale(alog10(fs(*,*,0)) $
	,alog10(mv(0)),alog10(mv(0)*10)) $
else begin
	print,'plot bands: 1+2 (red), 3 (green), and 4 (blue)'
	sfs(*,*,0)=bscale(alog10(fs(*,*,0)+fs(*,*,1)) $
	,alog10(mv(0)+mv(1)),alog10((mv(0)+mv(1))*10))
    endelse
if nb eq 2 then begin
  sfs(*,*,1)=bscale(alog10(fs(*,*,1)),alog10(mv(nb-2)),alog10(mv(nb-2)*10))
endif else begin
  sfs(*,*,1)=bscale(alog10(fs(*,*,nb-2)),alog10(mv(nb-2)),alog10(mv(nb-2)*10))
  sfs(*,*,2)=bscale(alog10(fs(*,*,nb-1)),alog10(mv(nb-1)),alog10(mv(nb-1)*10))
endelse

if keyword_set(noplot) eq 0 then begin
	window,xs=mdim(0),ys=mdim(1)
	cor=[0,1,0,1]
	cont_grey,sfs,hdr,cor=cor,true=3,mr=0,/ps,/nocon,/full,f_c=-1,barf=0.
	scale='1''
	scale_plot,cor,hdr,0.08,0.93,1,scale,thick=2,char=2,color=!d.n_colors-1
    endif
if !debug eq 2 then stop
return
end
