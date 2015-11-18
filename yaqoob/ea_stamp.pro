pro ea_stamp, obsname, loglun=loglun, date=date, echo=echo, $
      sis_xybin=sis_xybin, gis_xybin=gis_xybin, gisclean=gisclean

if n_params(0) eq 0 then begin
  print, 'ea_stamp, obsname, loglun=loglun, date=date, echo=echo, '
  print, '  sis_xybin=sis_xybin, gis_xybin=gis_xybin, gisclean=gisclean'
  print, 'Generates gif "stamp" images to show src. and bgd. region selections'
  retall
endif

if keyword_set(gisclean) then print, '/gisclean is set'
detname = ["s0", "s1", "g2", "g3"]
c = indgen(256)
if n_elements(loglun) eq 0 then loglun = -1
if n_elements(sis_xybin) eq 0 then sis_xybin = 4
if n_elements(gis_xybin) eq 0 then gis_xybin = 1
 
for det = 0, 3 do begin
  root = obsname + '_' + detname(det) + '_'
  srcreg = root + 'src.reg'
  bgdreg = root + 'bgd.reg'
  root = root + date
  if keyword_set(gisclean) and det ge 2 then root = root + '_giscl'
  evtfil = root + '_mkf.evt*'
  foundfiles = 1
  flist = findfile(srcreg, count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_stamp: could not find ' + srcreg
    if keyword_set(echo) then print, 'ea_stamp: could not find ' + srcreg
    foundfiles = 0
  endif
  flist = findfile(bgdreg, count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_stamp: could not find ' + bgdreg
    if keyword_set(echo) then print, 'ea_stamp: could not find ' + bgdreg
    foundfiles = 0
  endif
  flist = findfile(evtfil, count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_stamp: could not find ' + evtfil
    if keyword_set(echo) then print, 'ea_stamp: could not find ' + evtfil
    foundfiles = 0
  endif else evtfil = flist(0)
  if not foundfiles then begin
    printf, loglun, 'ea_stamp: skipping s' + strn(det) + $
      ' due to missing files'
    if keyword_set(echo) then print, 'ea_stamp: skipping s' + strn(det) + $
      ' due to missing files'
  endif else begin
    printf, loglun, 'ea_stamp: reading ' + evtfil
    if keyword_set(echo) then print, 'ea_stamp: reading ' + evtfil
    qreadasca, evtfil, plist
    if det le 1 then xybin = sis_xybin else xybin = gis_xybin
    im = bin_image_xy(plist.detx, plist.dety, bin=xybin)

    dx = round((plist.detx - 0.5)/xybin - 0.5)
    dy = round((plist.dety - 0.5)/xybin - 0.5)
    sz = max([dx,dy]) + 1
    im = intarr(sz, sz)
    for i=0l, n_elements(dx)-1 do im(dx(i), dy(i)) = im(dx(i), dy(i)) + 1 
    imscl = bytscl(alog10(im + 1))

    read_reg, srcreg, x, y, r
    for i=0, n_elements(x)-1 do begin
      dist_circle, circle, sz, x(i), y(i)
      w = where(circle ge fix(r(i)) and circle le fix(r(i)) + 1, n)
      imscl(w) = 255-imscl(w)
    endfor

    read_reg, bgdreg, x, y, r, bx, by, bwx, bwy
    r = abs(r)
    for i=0, n_elements(x)-1 do begin
      dist_circle, circle, sz, x(i), y(i)
      w = where(circle ge fix(r(i)) and circle le fix(r(i)) + 1, n)
      imscl(w) = 255-imscl(w)
    endfor
    if det le 1  then begin
      bwx = abs(bwx)/2
      bwy = abs(bwy)/2
      for i=0, n_elements(bx)-1 do begin
       xmin = fix(bx(i)-bwx(i)) > 0
       ymin = fix(by(i)-bwy(i)) > 0
       xmax = fix(bx(i)+bwx(i)) < sz-1
       ymax = fix(by(i)+bwy(i)) < sz-1
       if xmax gt xmin and ymax gt ymin then begin
; left
         imscl(xmin:xmin, ymin:ymax) = 255 - imscl(xmin:xmin, ymin:ymax)
; bottom
         imscl(xmin:xmax, ymin:ymin) = 255 - imscl(xmin:xmax, ymin:ymin)
; right
         imscl(xmax:xmax, ymin:ymax) = 255 - imscl(xmax:xmax, ymin:ymax)
; top
         imscl(xmin:xmax, ymax:ymax) = 255 - imscl(xmin:xmax, ymax:ymax)
       endif
      endfor
    endif
    write_gif, root + '_mkf_reg.gif', imscl(min(dx)-1:*, min(dy)-1:*), $
      255-c, 255-c, 255-c
    printf, loglun, 'ea_stamp: wrote ' + root + '_mkf_reg.gif'
    if keyword_set(echo) then print, 'ea_stamp: wrote ' + root + '_mkf_reg.gif'
  endelse
endfor

return
end
