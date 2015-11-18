pro ea_lc, obsname, bin=bin, mint=mint, maxsep=maxsep, septhresh=septhresh, $
  grpmin=grpmin, loglun=loglun, date=date, echo=echo, erange=erange, $
  gisclean=gisclean

if n_params(0) eq 0 then begin
  print, 'ea_lc, obsname, bin=bin, mint=mint, maxsep=maxsep, septhresh=septhresh, $'
  print, '  grpmin=grpmin, loglun=loglun, date=date, echo=echo, erange=erange
  print, 'Intended to be called from extr_asca'
  retall
endif

detnames = ['s0', 's1', 'g2', 'g3']
if n_elements(loglun) eq 0 then loglun = -1
sphalist = strarr(4)
bphalist = strarr(4)
sevtlist = strarr(4)
bevtlist = strarr(4)
gtilist = strarr(4)

for det=0, 3 do begin
  root = obsname + '_' + detnames(det) + "_" + date
  if keyword_set(gisclean) then if det ge 2 then root = root + "_giscl"
  src = root + '*src.pha'
  flist = findfile(src, count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_xspec: Cound not find any source spectra of the form ' $
      + src
    if keyword_set(echo) then print, $
      'ea_lc: Cound not find any source spectra of the form ' + src
    return
  endif

  if count ne 1 then begin
    printf, loglun, 'ea_lc: Found more than one source spectrum of the form ' $
      + src
    if keyword_set(echo) then print, $
      'ea_lc: Found more than one source spectrum of the form ' + src
    return
  endif

  sphalist(det) = flist(0)

  bgd = root + '*bgd.pha'
  flist = findfile(bgd, count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_lc: Cound not find any bgd. spectra of the form ' $
      + bgd
    if keyword_set(echo) then print, $
      'ea_lc: Cound not find any bgd. spectra of the form ' + bgd
    return
  endif

  if count ne 1 then begin
    printf, loglun, 'ea_lc: Found more than one bgd. spectrum of the form ' $
      + bgd
    if keyword_set(echo) then print, $
      'ea_lc: Found more than one bgd. spectrum of the form ' + bgd
    return
  endif

  bphalist(det) = flist(0)

  src = root + '*src.evt*'
  flist = findfile(src, count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_xspec: Cound not find any source events files of the form ' $
      + src
    if keyword_set(echo) then print, $
      'ea_lc: Cound not find any source events files of the form ' + src
    return
  endif

  if count ne 1 then begin
    printf, loglun, 'ea_lc: Found more than one source events file of the form ' $
      + src
    if keyword_set(echo) then print, $
      'ea_lc: Found more than one source events file of the form ' + src
    return
  endif

  sevtlist(det) = flist(0)

  bgd = root + '*bgd.evt*'
  flist = findfile(bgd, count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_lc: Cound not find any bgd. events files of the form ' $
      + bgd
    if keyword_set(echo) then print, $
      'ea_lc: Cound not find any bgd. events files of the form ' + bgd
    return
  endif

  if count ne 1 then begin
    printf, loglun, 'ea_lc: Found more than one bgd. events file of the form ' $
      + bgd
    if keyword_set(echo) then print, $
      'ea_lc: Found more than one bgd. events file of the form ' + bgd
    return
  endif

  bevtlist(det) = flist(0)

  gti = root + '*.gti'
  flist = findfile(gti, count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_lc: Cound not find any gti file of the form ' $
      + gti
    if keyword_set(echo) then print, $
      'ea_lc: Cound not find any gti file of the form ' + gti
  endif else begin

    if count ne 1 then begin
      printf, loglun, 'ea_lc: Found more than one gti file of the form ' $
        + gti
      if keyword_set(echo) then print, $
        'ea_lc: Found more than one gti file of the form ' + gti
      return
    endif

    gtilist(det) = flist(0)
  endelse

endfor

bscales = fltarr(4)
for det = 0, 3 do begin
  h = headfits(sphalist(det), ext=1)
  sarea = sxpar(h, 'backscal')
  h = headfits(bphalist(det), ext=1)
  barea = sxpar(h, 'backscal')
  bscales(det) = barea/sarea
endfor

; first create light curves for each instrument
for det=0, 3 do begin
  qreadasca, sevtlist(det), slist, sh, sgti
  qreadasca, bevtlist(det), blist, bh, bgti
  if gtilist(det) eq '' then gti = sgti else gti = read_fitsgti(gtilist(det))
  lcname = obsname + '_' + detnames(det) + '_' + date
  if det ge 2 and keyword_set(gisclean) then lcname = lcname + '_giscl' 
  if n_elements(bin) gt 0 then lcname = lcname + '_bin' + strn(bin) + 's'
  if n_elements(grpmin) gt 0 then lcname = lcname + '_grp' + strn(grpmin)
  if n_elements(erange) gt 0 then lcname = lcname + '_' + strn(erange(0), $
    format='(f6.2)') + '-' + strn(erange(1), format='(f6.2)') + 'kev'
  lcname = lcname + '.qdp'
  gtiflt, blist, gti, /silent
  gtiflt, slist, gti, /silent
  printf, loglun, lcname + ": bscale = " + strn(bscales(det))
  if keyword_set(echo) then print, lcname + ": bscale = " + strn(bscales(det))
  if n_elements(erange) gt 0 then begin
    blist = blist(select_e(blist, erange(0), erange(1), sis=det))
    slist = slist(select_e(slist, erange(0), erange(1), sis=det))
  endif
  qdplc_str = 'qdplc, slist, gti, lcname, bin=bin, maxsep=maxsep, septhresh=septhresh, mint=mint, blist=blist, bscale=bscales(det), grpmin=grpmin'

  printf, loglun, qdplc_str
  if keyword_set(echo) then print, qdplc_str
  if not execute(qdplc_str) then begin
    print, 'qdplc did not run successfully'
    return
  endif
endfor

; make a s0+s1 light curve
qreadasca, sevtlist(0), s0slist, s0sh, s0sgti
qreadasca, bevtlist(0), s0blist, s0bh, s0bgti
if gtilist(0) eq '' then s0gti = s0sgti else s0gti = read_fitsgti(gtilist(0))
qreadasca, sevtlist(1), s1slist, s1sh, s1sgti
qreadasca, bevtlist(1), s1blist, s1bh, s1bgti
if gtilist(1) eq '' then s1gti = s1sgti else s1gti = read_fitsgti(gtilist(1))
lcname = obsname + '_' + detnames(0) + detnames(1) + '_' + date
if n_elements(bin) gt 0 then lcname = lcname + '_bin' + strn(bin) + 's'
if n_elements(grpmin) gt 0 then lcname = lcname + '_grp' + strn(grpmin)
if n_elements(erange) gt 0 then lcname = lcname + '_' + strn(erange(0), $
  format='(f6.2)') + '-' + strn(erange(1), format='(f6.2)') + 'kev'
lcname = lcname + '.qdp'
if n_elements(erange) gt 0 then begin
  s0blist = s0blist(select_e(s0blist, erange(0), erange(1), sis=0))
  s0slist = s0slist(select_e(s0slist, erange(0), erange(1), sis=0))
  s1blist = s1blist(select_e(s1blist, erange(0), erange(1), sis=1))
  s1slist = s1slist(select_e(s1slist, erange(0), erange(1), sis=1))
endif
blist = [s0blist, s1blist]
slist = [s0slist, s1slist]
combtint, s0gti, s1gti, sgti
gtiflt, blist, sgti, /silent
gtiflt, slist, sgti, /silent
bscale = (bscales(0) + bscales(1))/2
print,'bscale/bscales(0:1) = ', bscale/bscales(0:1)
printf, loglun, lcname + ": bscale = " + strn(bscale)
if keyword_set(echo) then print, lcname + ": bscale = " + strn(bscale)
qdplc_str = 'qdplc, slist, sgti, lcname, bin=bin, maxsep=maxsep, septhresh=septhresh, mint=mint, blist=blist, bscale=bscale, grpmin=grpmin'
printf, loglun, qdplc_str
if keyword_set(echo) then print, qdplc_str
if not execute(qdplc_str) then begin
  print, 'qdplc did not run successfully'
  return
endif

; make a g2+g3 light curve
qreadasca, sevtlist(2), g2slist, g2sh, g2sgti
qreadasca, bevtlist(2), g2blist, g2bh, g2bgti
if gtilist(2) eq '' then g2gti = g2sgti else g2gti = read_fitsgti(gtilist(2))
qreadasca, sevtlist(3), g3slist, g3sh, g3sgti
qreadasca, bevtlist(3), g3blist, g3bh, g3bgti
if gtilist(3) eq '' then g3gti = g3sgti else g3gti = read_fitsgti(gtilist(3))
lcname = obsname + '_' + detnames(2) + detnames(3) + '_' + date
if keyword_set(gisclean) then lcname = lcname + '_giscl' 
if n_elements(bin) gt 0 then lcname = lcname + '_bin' + strn(bin) + 's'
if n_elements(grpmin) gt 0 then lcname = lcname + '_grp' + strn(grpmin)
if n_elements(erange) gt 0 then lcname = lcname + '_' + strn(erange(0), $
  format='(f6.2)') + '-' + strn(erange(1), format='(f6.2)') + 'kev'
lcname = lcname + '.qdp'
if n_elements(erange) gt 0 then begin
  g2blist = g2blist(select_e(g2blist, erange(0), erange(1)))
  g2slist = g2slist(select_e(g2slist, erange(0), erange(1)))
  g3blist = g3blist(select_e(g3blist, erange(0), erange(1)))
  g3slist = g3slist(select_e(g3slist, erange(0), erange(1)))
endif
blist = [g2blist, g3blist]
slist = [g2slist, g3slist]
combtint, g2gti, g3gti, ggti
gtiflt, blist, ggti, /silent
gtiflt, slist, ggti, /silent
bscale = (bscales(2) + bscales(3))/2
print,'bscale/bscales(2:3) = ', bscale/bscales(2:3)
printf, loglun, lcname + ": bscale = " + strn(bscale)
if keyword_set(echo) then print, lcname + ": bscale = " + strn(bscale)
qdplc_str = 'qdplc, slist, ggti, lcname, bin=bin, maxsep=maxsep, septhresh=septhresh, mint=mint, blist=blist, bscale=bscale, grpmin=grpmin'
printf, loglun, qdplc_str
if keyword_set(echo) then print, qdplc_str
if not execute(qdplc_str) then begin
  print, 'qdplc did not run successfully'
  return
endif

return
end

