pro ea_genreg, evtfil, srcreg, bgdreg, xybin=xybin, loglun=loglun, echo=echo,$
      sis_reg=sis_reg
if n_params(0) eq 0 then begin
  print, 'ea_genreg, evtfil, srcreg, bgdreg, xybin=xybin'
  print, 'Generate source and background regions based on evtfil'
  print, 'Intended to be called from extr_asca'
  retall
endif

if n_elements(loglun) eq 0 then loglun = -1

if n_elements(xybin) eq 0 then xybin = 1
printf, loglun, 'Spatial binning factor = ' + strn(xybin)

if n_elements(sis_reg) eq 0 then sis_reg = 4.

;ea_gunzip, [evtfil + '*']
qreadasca, evtfil(0), plist, h, gti
instr = strmid(sxpar(h, 'INSTRUME'), 0, 3)

; Bin image and get centroid of brightest pixel
dx = round((plist.detx - 0.5)/xybin - 0.5)
dy = round((plist.dety - 0.5)/xybin - 0.5)
sx = max(dx) + 1
sy = max(dy) + 1
im = intarr(sx, sy)

for i=0l, n_elements(dx)-1 do im(dx(i), dy(i)) = im(dx(i), dy(i)) + 1 
imsm = gauss_smooth(im, 2.)
w = where(imsm eq max(imsm), c)
if c gt 1 then begin
  print, 'Warning... more than one maximum in smoothed image'
endif
w = w(0)

y = w/sx
x = w - y*sx
cntrd, imsm, x, y, xc, yc, 8
; Now put (xc, yc) into fits convention
xc = xc + 1
yc = yc + 1
printf, loglun, "Source centroid = (" + strn(xc) + ", " + strn(yc) + ")"
if keyword_set(echo) then print, "Source centroid = (" + strn(xc) + ", " + strn(yc) + ")"

if instr eq 'SIS' then begin
;  src = ' CIRCLE(' + strn(xc) + ', ' + strn(yc) + ', ' + strn(37.7*4/xybin) $
;    + ')'
;  w = where((dx - xc)^2 + (dy - yc)^2 lt (2.*xybin)^2, c)
;  if c eq 0 then stop
;  ccd = avg(plist(w).ccd)
;  w = where(plist.ccd eq ccd)
;  rx = minmax(dx(w))
;  ry = minmax(dy(w))
;  midx = avg(rx)
;  midy = avg(ry)
;  xsize = rx(1) - rx(0)
;  ysize = ry(1) - ry(0)
;  bgd = [' BOX(' + strn(midx) + ', ' + strn(midy) + ', ' + strn(xsize) + ', ' $
;    + strn(ysize) + ')', '-CIRCLE(' + strn(xc) + ', ' + strn(yc) + ', ' + $
;    strn(37.7*5/xybin) + ')']
   sis = fix(strmid(sxpar(h, 'INSTRUME'), 3, 1))
   make_sis_reg, plist, (xc-0.5)*xybin-0.5, (yc-0.5)*xybin-0.5, 'temp', $
     xybin, sis_reg, sis_reg+1, sis=sis
   spawn, 'mv temp_src_' + strn(sis_reg, f='$(f5.2)') + 'min.reg ' + srcreg
   spawn, 'mv temp_bkg_' + strn(sis_reg+1, f='$(f5.2)') + 'min.reg ' + bgdreg
endif else begin
  src = ' CIRCLE(' + strn(xc) + ', ' + strn(yc) + ', ' + strn(4.072*6/xybin) $
    + ')'
  bgd = [' CIRCLE(' + strn(xc) + ', ' + strn(yc) + ', ' + $
    strn(4.072*12/xybin) + ')', '-CIRCLE(' + strn(xc) + ', ' + strn(yc) + $
    ', ' + strn(4.072*7/xybin) + ')']
  openw, lun, srcreg, /get_lun
  printf, lun, '# Source region generated by ea_genreg.pro on ' + $
    systime()
  printf, lun, src
  close, lun
  openw, lun, bgdreg
  printf, lun, '# Backgrd. region generated by ea_genreg.pro on ' + $
    systime()
  printf, lun, bgd(0)
  printf, lun, bgd(1)
  free_lun, lun
endelse


return
end