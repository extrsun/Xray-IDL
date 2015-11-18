pro get_mask,width,mask,ourfile=outfile
;-
; produce a mask with regions covered by ribs set to sero values
; and others set to 1
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_mask,width,mask,ourfile=outfile'
return
endif

if n_elements(outfile) eq 0 then outfile='mask.fits'
	mapnorm_B=[12.9761E-5, 4.6281E-5, 6.0585E-5, 5.0370E-5, 5.6676E-5, $
			 5.4558E-5, 5.9294E-5]
	mapnorm_C=[15.8361E-5, 8.1814E-5, 6.3375E-5, 5.0491E-5, 5.0294E-5, $
			 4.9386E-5, 5.3749E-5]
scale=mapnorm_B(3)
tscale=1./scale
map=readfits('exposure_52_69_B.fits',hdr)
        PRINT,'Centering instrument map...'
;	I=INDGEN(463) ;compared with the original fortran program, 463 is not right
	I=INDGEN(464)
	I=I+24
	RMAP=FLTARR(512,512)
       	RMAP(*,512-I)=tscale*MAP(*,I-12) ;Is the orientation right?
;	rmap=shift(rmap,1,-1) ;provides the best match to the *_mex.fits
mask=rmap
loc=lindgen(512,512)
xloc=loc mod 512L
yloc=loc/512L
minpix=4.
hw=minpix*width*0.5
cp=255.5
dist_circle,dis,512,cp,cp
c=where(rmap le 0.)
mask(c)=0.
c=where(dis gt 84.-hw and dis lt 84.+hw)
mask(c)=0.
c=where(dis ge 80.+hw and $
	(abs(xloc-cp) lt hw or abs(yloc-cp) lt hw))
mask(c)=0.
mrot=rot(mask,45.,1.,256.,256.,missing=0.)
mrot(c)=0.
mask=rot(mrot,45.,1.,256.,256.,missing=0.) ;flip total 180 degree
b=mask*0.
b(where(mask gt 0.))=1.
mask=b
;shift back to the original position, the flipping is not neccessary here
; because of the sysmetric property of the mask
mask=shift(mask,0,-11) ;the center of the detector map is at 257.4,245.6
;see Steve's note which corresponds a shift 11.4 (if 256 is considered to the
; image center
writefits,outfile,mask,hdr
stop
end
