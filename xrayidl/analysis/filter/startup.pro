;
; read image, exposure maps
;
im = readfits('count.fits')
ee = readfits('exp.fits')
ee0 = readfits('exp0.fits')
ee(where(ee LE 0)) = 0
ee0(where(ee0 LE 0)) = 0
;
; ff is "flat-field" map -- has mean 1
;
ww = where(ee GT 0)
ff = ee*n_elements(ww)/total(ee(ww))
;
; typical filtering command -- this will take about 80 seconds on
; a sparc-2 equivalent
;
; using exptime=ff gives "flat-fielded counts"
; using exptime=ee instead would give cts/s
;
; pim = pfilter(im,2.0,5,exptime=ff)

