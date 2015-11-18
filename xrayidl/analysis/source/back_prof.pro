pro back_prof,image_c,dist,flux,loflux,hiflux,choice=choice, $
block=block,radiuslow=radiuslow,radiushi=radiushi,filter=filter $
,siglevel=siglevel,refx=refx,refy=refy,nmin=nmin,sambin=sambin,ee=ee,pa=pa
;+
; Calaculate the radial surface brightness profile of an image
; 
;Inputs:
; image_c - image
;
;Optional Input:
; choice - 1: normal average, 2: median average (def)
; block - block size of the image pixel (def=!block)
; radiuslow, radiushi - lower and upper limits of the radial range.
; 			(def =0 to infinite)
; filter - image with non-zero pixels will be selected in the calculation
; siglevel - for the lower and upper limits if choice =2 
; nmin - the minimum number of image pixesl needed for the calculation
;	per interval if choice =2; def=10
; refx, refy - the reference center image pixel values in IDL format
;		def: (sz(0)-1)*0.5, (sz(1)-1)*0.5
; sambin - the flux conversion (def = counts/s arcmin^2)
;
;Outputs:
; dist - off-center distance
; flux, loflux, hiflux - mean flux and its lower and upper limits (the limits
; 	are only calculated for choice=2 (median average)
; 
; written by wqd, 11/30/2003
;-		
;---------------------------------------------------------------
if n_params() EQ 0 then begin
print,'CALL SEQUENCE - back_prof,image_c,dist,flux,loflux,hiflux'
print,',choice=choice,block=block,radiuslow=radiuslow,radiushi=radiushi'
print,',filter=filter,siglevel=siglevel,refx=refx,refy=refy,nmin=nmin,sambin=sambin,ee=ee,pa=pa'
retall
endif
;---------------------------------------------------------------
if n_elements(nmin) eq 0 then nmin=10
if n_elements(choice) eq 0 then  choice=2
if n_elements(block) EQ 0 then block=!block
sz=size(image_c) 
szmin=[sz(1),sz(2)]
if n_elements(radiuslow) eq 0 then rlow=0. else rlow=radiuslow/(!size_pixel*block)

if n_elements(radiushi) eq 0 then rhi=1.e10 else rhi=radiushi/(!size_pixel*block)
if n_elements(refx) eq 0 then refx=(sz(1)-1)/2.
if n_elements(refy) eq 0 then refy=(sz(2)-1)/2.
print,'Input number of radial divisions:' & read,ndivs
ringwidth=(rhi-rlow)/ndivs
;
flux=fltarr(ndivs)
loflux=fltarr(ndivs)
hiflux=loflux
dist=flux
;
if n_elements(ee) ne 0 then $
  dist_ellipse,circle,szmin(0),refx,refy,ee,-pa*!pi/180.,dimy=szmin(1) $
  else dist_circle,circle,szmin,refx,refy
  if n_elements(filter) eq 0 then $
    good=where((circle ge rlow) and (circle LT rhi), n_bin) $
    else good=where((circle ge rlow) and (circle LT rhi) and $
                    filter gt 0., n_bin)
  if n_bin eq 0 then begin
	print,'No data covered in the radius. Is the radius too small?'
  endif
;
  dis=circle(good)
  vec_c=float(image_c(good))
  ss=sort(dis)
  dis=dis(ss)
  rad=fix((dis-rlow)/ringwidth)
  vec_c=vec_c(ss)

  for n=0L,ndivs-1 do begin
	ss=where(rad eq n,nss)
	if nss gt nmin then begin
            if choice eq 1 then flux(n)=avg(vec_c(ss)) $
            else begin
		avg_median,vec_c(ss),fm,flo,fhi,siglevel=siglevel
		flux(n)=fm
		loflux(n)=flo
		hiflux(n)=fhi
		;flux(n)=median(vec_c(ss))
	    endelse 
            dist(n)=total(vec_c(ss)*dis(ss))/total(vec_c(ss))
            ;flux_lo(n)=min(vec_c(ss))
        endif 
  endfor

ambin=60./(block*!size_pixel)
if n_elements(sambin) eq 0 then  sambin=ambin^2
sel=where(dist ne 0.)
dist=dist(sel)/ambin
flux=flux(sel)*sambin
loflux=loflux(sel)*sambin
hiflux=hiflux(sel)*sambin
if rlow eq 0 then dist(0)=0.
;
if !debug eq 3 then stop
return
end 
