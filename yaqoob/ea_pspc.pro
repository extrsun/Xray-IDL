pro ea_pspc, pspcfiles, obsname, loglun=loglun, date=date, echo=echo, $
    sis_xybin=sis_xybin, gis_xybin=gis_xybin, gisclean=gisclean, $
    recenter=recenter
;ea_pspc,'n4579_pspc_files.dat','n4579',date='03jan97',sis_xybin=4,/giscl
;extr_asca,'n4579',date='03jan97',/giscl,/clean,/echo,$
;log='n4579_ea_pspc_20mar97.log',pspc='n4579_pspc_files.dat'
if n_params(0) eq 0 then begin
  print, 'ea_pspc, pspcfiles, obsname, loglun=loglun, date=date, echo=echo
  print, 'Determines source position in each pspc evt. file and then extracts
  print, " 1.25 and 4' src. spectra and a 8-13' bgd. spectrum"
  print, 'Intended to be called from extr_asca'
  retall
endif 

detnames = ['s0', 's1', 'g2', 'g3']
if n_elements(loglun) eq 0 then loglun = -1
if n_elements(sis_xybin) eq 0 then sis_xybin=1
if n_elements(gis_xybin) eq 0 then gis_xybin=1
 
printf, loglun, ''
printf, loglun, '--------------------------------------------------------------
printf, loglun, 'PSPC'
printf, loglun, '--------------------------------------------------------------
printf, loglun, ''
if keyword_set(echo) then begin
  print, ''
  print, '--------------------------------------------------------------
  print, 'PSPC'
  print, '--------------------------------------------------------------
  print, ''
endif
 
; Centroid ra, dec of 4 asca dets.
ra = dblarr(4)
dec = dblarr(4)

; First get a list of pspc event files to be processed
openr, lun, pspcfiles, /get_lun
a = ''
npspc = 0
while not eof(lun) do begin
  readf, lun, a
  last_slash = rstrpos(a, '/') + 1
  first_dot = strpos(a, '.', last_slash)
  if first_dot eq -1 then first_dot = strlen(a)
  pspcroot = strmid(a, last_slash, first_dot-last_slash)
  xco = pspcroot + '_spec.xco'
  flist = findfile(xco, count=count)
  if count gt 0 then begin
    printf, loglun, xco + $
      ' exists, delete it if you want the PSPC spectra to be re-extracted'
    if keyword_set(echo) then print, xco + $
      ' exists, delete it if you want the PSPC spectra to be re-extracted'
  endif else begin
    if npspc eq 0 then begin
      xcolist = xco
      pspclist = a
      pspcroots = pspcroot
    endif else begin
      xcolist = [xcolist, xco]
      pspclist = [pspclist, a]
      pspcroots = [pspcroots, pspcroot]
    endelse
    npspc = npspc + 1
  endelse
endwhile

if npspc eq 0 then begin
  printf, loglun, 'No PSPC spectra extra to be extracted, returning'
  if keyword_set(echo) then print, 'No PSPC spectra extra to be extracted, returning'
  return
endif
 
; Determine the ASCA source position in sky coordinates
if n_elements(obsname) gt 0 then begin
  for det=0, 3 do begin
    root = obsname + '_' + detnames(det)
    srcreg = root + '_src.reg'
    fil = findfile(srcreg, count=count)
    if count eq 0 then begin
      printf, loglun, 'ea_pspc: Could not find source region file ' + srcreg
      if keyword_set(echo) then print, $
        'ea_pspc: Could not find source region file ' + srcreg
      return
    endif

  ; Read src. pos., put in unbinned coords
    if det le 1 then xybin = sis_xybin else xybin = gis_xybin
    read_reg, srcreg, detx, dety, rad, bin=xybin

    if det eq 0 then begin
      pspcrad = rad(0)/37.7
      print, 'PSPC source region size will be ' + strn(pspcrad) + "'"
    endif
    printf, loglun, srcreg + ' position = (' + strn(detx) + ', ' + strn(dety) + $
      ')'
    if keyword_set(echo) then print, srcreg + ' position = (' + strn(detx) + $
      ', ' + strn(dety) + ')'

  ; Now read in event files to get convert to sky pos.
    root = root + '_' + date
    if keyword_set(gisclean) then if det ge 2 then root = root + "_giscl"
    evtfil = findfile(root + '_mkf.evt*', count=count)
    if count eq 0 then begin
      printf, loglun, 'ea_pspc: Could not find source event file ' + root + $
        '_mkf.evt*'
      if keyword_set(echo) then print, $
        'ea_pspc: Could not find source event file ' + evtfil + '_mkf.evt*'
      return
    endif

    print, 'Reading ' + evtfil(0)
    qreadasca, evtfil(0), plist, h, gti
    w = where(abs(plist.detx - detx(0)) le 25 and abs(plist.dety - dety(0)) $
              le 25)
    skyx = avg(plist(w).skyx)
    skyy = avg(plist(w).skyy)
    printf, loglun, 'sky x,y = (' + strn(skyx) + ', ' + strn(skyy) + ')'
    if keyword_set(echo) then print, 'sky x,y = (' + strn(skyx) + $
      ', ' + strn(skyy) + ')'

  ; Bin up sky image, if nec.
    flist = findfile(root + '_mkf_sky.img' + '*', count=count)
    if count eq 1 then img = readfits(flist(0), hdr) else $
      make_fits_image, evtfil(0), root + '_mkf_sky.img', bin=xybin, $
        plist=plist, img=img, hdr=hdr

  ; Find centroid of sky image and get ra, dec
    img_sm = gauss_smooth(img, 2.)
    cntrd, img_sm, skyx/xybin, skyy/xybin, newskyx,newskyy, 8.
    extast, hdr, astr
    xy2ad, newskyx, newskyy, astr, a, d
    adstr = adstring(a, d)
    printf, loglun, detnames(det) + ' source position: ' + adstr
    if keyword_set(echo) then print, detnames(det) + ' source position: ' + $
      adstr
    ra(det) = a
    dec(det) = d
  endfor

  sa = stdev(ra, ma)
  sd = stdev(dec, md)
  adstr = adstring(ma, md)
  printf, loglun, 'Mean source position: ' + adstr
  if keyword_set(echo) then print, 'Mean source position: ' + adstr
  printf, loglun, 'RA std. dev. = ' + strn(sa*cos(md/!radeg)*3600) + '"'
  printf, loglun, 'DEC std. dev. = ' + strn(sd*3600) + '"'
  if keyword_set(echo) then begin
    print, 'RA std. dev. = ' + strn(sa*cos(md/!radeg)*3600) + '"'
    print, 'DEC std. dev. = ' + strn(sd*3600) + '"'
  endif
  asca = 1
endif else asca = 0

; Now get centroid pos. for PSPC image(s)
get_lun, lun2 ; get log. unit for region files
get_lun, templun ; get log. unit for temporary file to drive pcpicor

for i=0, npspc-1 do begin

; We have to make sure the file is not compressed    
  last_dot = rstrpos(pspclist(i), '.') + 1
  ext = strmid(pspclist(i), last_dot, strlen(pspclist(i))-last_dot)
  if ext eq 'gz' or ext eq 'Z' then spawn, 'gunzip ' + pspclist(i) else $
    last_dot = strlen(pspclist(i)) + 1
  gunzipfil = strmid(pspclist(i), 0, last_dot-1)

; First see if we need to run pcpicor
; Get events extension header
  evtn = 1
  a = ''
  while strmid(a,0,6) ne 'EVENTS' and strmid(a,0,6) ne 'STDEVT' do begin
    h = headfits(gunzipfil, ext=evtn)
    a = sxpar(h, 'EXTNAME')
    print, a
    evtn = evtn+1
  endwhile
  evtn = evtn-1
  if sxpar(h, 'PCECORF') ne 1 then begin
      stdin = 'ea_pspc_pcpicor_drive_' + pspcroots(i) + '.dat'
      openw, templun, stdin
      printf, loglun, stdin + ' contains:'
      printf, templun, gunzipfil
      printf, loglun, gunzipfil
      printf, templun, 'y'
      printf, loglun, 'y'
      pspcroots(i) = pspcroots(i) + '_fix'
      gunzipfil = pspcroots(i) + '.fits'
      printf, templun, gunzipfil
      printf, loglun, gunzipfil
      printf, templun, 'yes'
      printf, loglun, 'yes'
      close, templun
      cmd = 'pcpicor < ' + stdin
      printf, loglun, cmd
      if keyword_set(echo) then print, cmd
      spawn, cmd, result, count=count
      for j=0, count-1 do printf, loglun, result(j)
      if keyword_set(echo) then for j=0, count-1 do print, result(j)
  endif else begin
      printf, loglun, "pcpicor was probably already run on " + gunzipfil
      if keyword_set(echo) then print, "pcpicor was probably already run on " $
        + gunzipfil
  endelse

  flist = findfile(pspcroots(i)+'_bin15.img', count=count)
  if count eq 1 then img=readfits(pspcroots(i)+'_bin15.img', hdr) else $
    make_fits_image, gunzipfil, pspcroots(i)+'_bin15.img', bin=15, img=img, $
      hdr=hdr
  print, 'Computing source centroid for ' + pspcroots(i)+ '_bin15.img...'
  extast, hdr, astr
  if asca then begin
    ad2xy, ma, md, astr, x, y
  endif else begin
    x = 512.
    y = 512.
  endelse
  img_sm = gauss_smooth(img, 2.)
  if keyword_set(recenter) then begin
    print, 'Determining image peak...'
    subim = img_sm(x-100:x+100, y-100:y+100) 
    w = where(subim eq max(subim), c)
    if c gt 1 then begin
      print, 'Warning... more than one maximum in smoothed image'
    endif
    w = w(0)
    sx = 201
    newy = w/sx
    newx = w - newy*sx
    newx = newx + x - 100.5
    newy = newy + y - 100.5
    printf, loglun, 'New pspc peak position = (' + strn(newx) + ', ' + $
      strn(newy) + ')'
    if keyword_set(echo) then print, 'New pspc peak position = (' + strn(newx) + ', ' + $
      strn(newy) + ')'
    stop
    x = newx
    y = newy
  endif
  cntrd, img_sm, x, y, xc, yc, 8.
  off = sqrt((xc-512.)^2 + (yc-512.)^2)/8.
  if asca then begin
    printf, loglun, obsname + ' is ~ ' + strn(off, f='$(f6.2)') + "' off-axis"
    if keyword_set(echo) then print, obsname + ' is ~ ' + $
      strn(off, f='$(f6.2)') + "' off-axis" 
  endif
  if off gt 10. then begin
    printf, loglun, 'Warning... large source radii are nec. for off-axis obs.'
    if keyword_set(echo) then print, $
      'Warning... large source radii are nec. for off-axis obs.'
  endif
  xy2ad, xc, yc, astr, ra, dec
  ad_str = adstring(ra, dec)
  printf, loglun, pspcroot + ' position = ' + ad_str
  if keyword_set(echo) then print, pspcroot + ' position = ' + ad_str
  if asca then begin
    offra = (ma-ra)*cos(dec/!radeg)*3600
    offdec = (md-dec)*3600
    printf, loglun, 'ASCA - PSPC offset = ' + strn(offra) + '", ' + $
      strn(offdec) + '"'
    if keyword_set(echo) then print, 'ASCA - PSPC offset = ' + strn(offra) + $
      '", ' + strn(offdec) + '"'
  endif else pspcrad = 1.25
  reg = pspcroots(i) + '_src_1.25min.reg'
  if asca then begin
    printf, loglun, 'Writing ' + reg
    if keyword_set(echo) then print, 'Writing ' + reg
    openw, lun2, reg
    printf, lun2, ' CIRCLE(' + strn(xc+1) + ', ' + strn(yc+1) + ', 10.)'
    close, lun2
  endif
  reg = pspcroots(i) + '_src_'+strn(pspcrad, f='$(f5.2)')+'min.reg'
  printf, loglun, 'Writing ' + reg
  if keyword_set(echo) then print, 'Writing ' + reg
  openw, lun2, reg
  printf, lun2, ' CIRCLE(' + strn(xc+1) + ', ' + strn(yc+1) + ', ' + $
    strn(pspcrad*8) + ')'
  close, lun2
  reg = pspcroots(i) + '_bgd_'+strn(pspcrad+1, f='$(f5.2)')+'-'+$
    strn(pspcrad+6, f='$(f5.2)') + 'min.reg'
  printf, loglun, 'Writing ' + reg
  if keyword_set(echo) then print, 'Writing ' + reg
  openw, lun2, reg
  printf, lun2, ' CIRCLE(' + strn(xc+1) + ', ' + strn(yc+1) + ', ' + $
    strn((pspcrad+6)*8) + ')'
  printf, lun2, '-CIRCLE(' + strn(xc+1) + ', ' + strn(yc+1) + ', ' + $
    strn((pspcrad+1)*8) + ')'
  close, lun2

; make nifty stamp images showing regions
  printf,loglun, 'Creating stamp image'
  if keyword_set(echo) then print, 'Creating stamp image'
  subim = alog10(img(xc-100:xc+100, yc-100:yc+100)+1)
  bsubim = bytscl(subim)
  dist_circle, circle, 201, 100+xc-fix(xc), 100+yc-fix(yc)
  if asca then begin
    w = where(circle ge 9.5 and circle le 10.5)
    bsubim(w) = 255 - bsubim(w)
  endif
  w = where(circle ge (pspcrad*8.)*0.99 and circle le (pspcrad*8.)*1.01)
  bsubim(w) = 255 - bsubim(w)
  w = where(circle ge ((pspcrad+1)*8.)*0.99 and circle le $
    ((pspcrad+1)*8.)*1.01)
  bsubim(w) = 255 - bsubim(w)
  w = where(circle ge ((pspcrad+6)*8.)*0.99 and circle le $
    ((pspcrad+6)*8.)*1.01)
  bsubim(w) = 255 - bsubim(w)
  color = indgen(256)
  write_gif, pspcroots(i) + '_stamp.gif', bsubim, 255-color, 255-color,$
    255-color

;  if off lt 10. then begin
; If pspc gti files do not exist, see create them
  if not keyword_set(noclean) then begin
    gtifil = pspcroots(i) + '.gti'
    flist = findfile(gtifil, count=count)
    if count eq 0 then begin
      printf, loglun, 'Cleaning background lightcurve...'
      if keyword_set(echo) then print, 'Cleaning background lightcurve...'
      qreadpspc, gunzipfil, plist, gti
      dsq = ((plist.x-1)/15+1.-xc)^2+((plist.y-1)/15+1.-yc)^2
      plist = plist(where(dsq ge (8.*(pspcrad+1))^2 and dsq le $
        (8.*(pspcrad+6))^2))
      ngti = (size(gti))(1)

  ; Arbitrarily define an obi as a set of gti separated by no more that 5 hours
      wobi = where(gti(1:*,0)-gti(0:ngti-2,1) ge 18000, nobi)
      if nobi eq 0 then begin
        wobi = ngti-1
      endif
      obimap = intarr(nobi+1,2)
      obimap(0,1) = wobi(0)
      for obi=1, nobi-1 do begin
        obimap(obi, 0) = obimap(obi-1, 1)+1
        obimap(obi, 1) = wobi(obi)
      endfor
      if nobi gt 0 then obimap(nobi, 0) = obimap(nobi-1, 1)+1
      obimap(nobi, 1) = ngti-1
      for obi=0, nobi do begin
        cgti = gti(obimap(obi, 0):obimap(obi, 1),*)
        sigclean, plist, cgti, cgticl, bin=128, /make_qdp, qdpin=pspcroots(i) + $
          '_obi'+strn(obi+1)+'_in.qdp', qdpout=pspcroots(i) + '_obi' + strn(obi+1) + $
          '_out.qdp', loglun=loglun
        if obi eq 0 then mgti = cgticl else mgti = [mgti, cgticl]
      endfor
      t1 = texp(gti)
      t2 = texp(mgti)
      printf, loglun, 'Exposure time before/after = ' + strn(t2) + '/' + $
        strn(t1) + ' = ' + strn(t2/t1)
      printf, loglun, 'Change in exposure time = ' + strn(t1-t2) + ' s'
      if keyword_set(echo) then begin
        print, 'Exposure time before/after = ' + strn(t2) + '/' + strn(t1) + $
          ' = ' + strn(t2/t1)
        print, 'Change in exposure time = ' + strn(t1-t2) + ' s'
      endif
      write_gtifits, gtifil, mgti
    endif
  endif



; Write xselect scripts to read pspc files
  openw, lun2, xcolist(i)
  printf, lun2, 'xsel'
  printf, lun2, 'set mission ROSAT'
  printf, lun2, 'set instr PSPC'
  printf, lun2, 'read eve ' + gunzipfil + ' .'
  if not keyword_set(noclean) then printf, lun2, 'filter time file ' + gtifil
  if asca then begin
    printf, lun2, 'filter region ' + pspcroots(i) + '_src_1.25min.reg'
    printf, lun2, 'extr spec'
    printf, lun2, 'save spec ' + pspcroots(i) + $
      '_src_1.25min.pha group=no clobber=yes'
    printf, lun2, 'clear region'
  endif
  regroot = pspcroots(i) + '_src_'+strn(pspcrad, f='$(f5.2)')+'min'
  printf, lun2, 'filter region ' + regroot + '.reg'
  printf, lun2, 'extr spec'
  printf, lun2, 'save spec ' + regroot + '.pha group=no clobber=yes'
  printf, lun2, 'clear region'
  regroot = pspcroots(i) + '_bgd_'+strn(pspcrad+1, f='$(f5.2)')+'-'+$
    strn(pspcrad+6, f='$(f5.2)') + 'min'
  printf, lun2, 'filter region ' + regroot + '.reg'
  printf, lun2, 'extr spec'
  printf, lun2, 'save spec ' + regroot + '.pha group=no clobber=yes'
  printf, lun2, 'exit'
  printf, lun2, 'no'
  close, lun2
  spawn, 'xselect @' + xcolist(i)

; Bin spectra with correct rmf or rsp
; Check gain state
;  h = headfits(gunzipfil, ext=2)
;  gain = (sxpar(h, 'tstart') ge 42910000.) + 1
  gti = read_fitsgti(gunzipfil)
  gain = (min(gti) ge 42910000.) + 1
  rmfroot = '/FTP/caldb/data/rosat/pspc/cpf/matrices/pspcb_gain' + strn(gain) $
    + '_256'
; Do we need to make an arf?
  arf = 'none'
  resp = rmfroot + '.rsp'
  if off ge 10. then begin
;    resp = rmfroot + '.rmf'
;    specresp = '/FTP/caldb/data/rosat/pspc/cpf/pspcb_v2.spec_resp'
    regroot = pspcroots(i) + '_src_1.25min'
    arf = regroot + '.arf'
    cmd = 'pcarf ' + regroot + '.pha CALDB ' $
      + arf + ' CALDB'
    printf, loglun, cmd
    if keyword_set(echo) then print, cmd
    spawn, cmd
    regroot = pspcroots(i) + '_src_'+strn(pspcrad, f='$(f5.2)')+'min'
    arf = regroot + '.arf'
    cmd = 'pcarf ' + regroot + '.pha CALDB ' $
      + arf + ' CALDB'
    printf, loglun, cmd
    if keyword_set(echo) then print, cmd
    spawn, cmd
    h = headfits(arf, ext=1)
    resp = sxpar(h, 'RESPFILE')
  endif

  regroot = pspcroots(i) + '_src_'+strn(pspcrad, f='$(f5.2)')+'min'
  grp_str = 'grppha ' +  regroot + '.pha' + ' ' + regroot + '_b20.pha ' + $
    'comm = "bad 1-11 211-256 & group min 20 & chkey resp ' + resp + $
    ' & exit "'
  printf, loglun, grp_str
  if keyword_set(echo) then print, grp_str
  flush, loglun
  spawn, grp_str

  if asca then begin
    regroot = pspcroots(i) + '_src_1.25min'
    grp_str = 'grppha ' +  regroot + '.pha' + ' ' + regroot + '_b20.pha ' + $
      'comm = "bad 1-11 211-256 & group min 20 & chkey resp ' + resp + $
      ' & exit "'
    printf, loglun, grp_str
    if keyword_set(echo) then print, grp_str
    flush, loglun
    spawn, grp_str
  endif
  spawn, 'gzip -v ' + gunzipfil

endfor

free_lun, lun2, lun, templun
return
end



