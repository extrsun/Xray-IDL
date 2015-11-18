pro plot_stars,ir0h,star_ra,star_dec,corner,starsym,star_label=star_label, $
  color=color,charthick=charthick,charsize=charsize
;-
;**
;** Plot location of stars in cont_grey image.
;**
;** INPUTS:
;**		ir0h	 == FITS header of cont_grey image;
;**		star_ra	 == vector containing RA of stars;
;**		star_dec == vector containing DEC of stars;
;**		corner	 == cont_grey output containing normalized
;**			    coordinates of image corners;
;**		starlabel == labels for stars.
;**
;** Written by kachun 24 July, 1994.
;+
if n_params() eq 0 then begin
print,'calling sequence - plot_stars,ir0h,star_ra,star_dec,corner,starsym'
print,',star_label=star_label,color=color,charthick=charthick,charsize=charsize'
return
endif
if n_elements(charsize) eq 0 then charsize=1
if n_elements(charthick) eq 0 then charthick=1
if n_elements(color) eq 0 then color = replicate(241,n_elements(star_ra))
nstar = n_elements(star_ra)

crval = sxpar(ir0h,'crval*')                 ;** Get IRAS image center.
naxis1 = sxpar(ir0h,'naxis1')
naxis2 = sxpar(ir0h,'naxis2')
irra = crval(0)
irdec = crval(1)
trans_dist,irra,irdec,star_ra,star_dec,sxo,syo,/deg
crpix = sxpar(ir0h,'crpix*')                 ;** Reference center.
cdelt = sxpar(ir0h,'cdelt*')
sx = sxo/(7200.*abs(cdelt(0)))+crpix(0)      ;** In units of IRAS pixel.
sy = syo/(7200.*abs(cdelt(1)))+crpix(1)
nx1 = corner(0) & nx2 = corner(1)
ny1 = corner(2) & ny2 = corner(3)
dx = nx2 - nx1  & dy = ny2 - ny1
sxn = nx1 + dx*sx/(naxis1) & syn = ny1 + dy*sy/(naxis2)

!p.thick=4.
!p.charsize=1.9
!p.charthick=2.00
if n_elements(star_label) ne 0 then $
  for i=0,nstar-1 do plots,sxn(i),syn(i),psym=starsym(i),/normal,color=color(i)
  for i=0,nstar-1 do xyouts,sxn(i),syn(i)+dy/100.,/normal,star_label(i), $
    alignment=0.5,color=color(i),charthick=charthick,charsize=charsize
!p.thick=1.
!p.charsize=1.
!p.charthick=1.

return
end
