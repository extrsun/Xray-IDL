pro ea_xspec, xspecflag, obsname, z=z, loglun=loglun, date=date, echo=echo, at=at, root=root, oldxcm=oldxcm, gisclean=gisclean

if n_params(0) eq 0 then begin
  print, 'ea_xspec, xspecflag, obsname, z=z, loglun=loglun, date=date, echo=echo, at=at, root=root, oldxcm=oldxcm, gisclean=gisclean'
  print, 'Generates xcm files for setting up xspec fits
  print, 'Intended to be called from extr_asca'
  retall
endif

detnames = ['s0', 's1', 'g2', 'g3']
if n_elements(loglun) eq 0 then loglun = -1

s0_1_hdr = strarr(8)
s0_3_hdr = strarr(14)
rmf_files = strarr(2)
rmg = (xspecflag and 2) eq 2

s0_1_hdr(2) = "ig bad"
s0_1_hdr(7) = "chat 0"
s0_3_hdr(4) = "ig bad"
s0_3_hdr(13) = "chat 0"

for det=0, 3 do begin
  src = obsname + '_' + detnames(det) + "_" + date
  if keyword_set(gisclean) then if det ge 2 then src = src + "_giscl"
  src = src + '*src_b??.pha'
  flist = ea_fingun(src + '*', count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_xspec: Cound not find any source spectra of the form ' $
      + src
    if keyword_set(echo) then print, $
      'ea_xspec: Cound not find any source spectra of the form ' + src
    return
  endif

  if count ne 1 then begin
    printf, loglun, 'ea_xspec: Found more than one source spectrum of the form ' $
      + src
    if keyword_set(echo) then print, $
      'ea_xspec: Found more than one source spectrum of the form ' + src
    return
  endif

  temp  = "data " + strn(det+1) + ":" + strn(det+1) + " " + flist(0)
  if det le 1 then s0_1_hdr(det) = temp
  s0_3_hdr(det) = temp

  bgd = obsname + '_' + detnames(det) + "_" + date
  if keyword_set(gisclean) then if det ge 2 then bgd = bgd + '_giscl'
  bgd = bgd + '*bgd.pha'
  flist = ea_fingun(bgd + '*', count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_xspec: Cound not find any bgd. spectra of the form ' $
      + bgd
    if keyword_set(echo) then print, $
      'ea_xspec: Cound not find any bgd. spectra of the form ' + bgd
    return
  endif

  if count ne 1 then begin
    printf, loglun, 'ea_xspec: Found more than one bgd. spectrum of the form '$
      + bgd
    if keyword_set(echo) then print, $
      'ea_xspec: Found more than one bgd. spectrum of the form ' + bgd
    return
  endif

  temp  = "back " + strn(det+1) + " " + flist(0)
  if det le 1 then s0_1_hdr(3 + det) = temp
  s0_3_hdr(5 + det) = temp

  arf = obsname + '_' + detnames(det) + "_" + date + '*arf'
  flist = ea_fingun(arf + '*', count=count)
  if count eq 0 then begin
    printf, loglun, 'ea_xspec: Cound not find any arf files of the form ' $
      + arf
    if keyword_set(echo) then print, $
      'ea_xspec: Cound not find any arf files of the form ' + arf
    return
  endif

  if count ne 1 then begin
    printf, loglun, 'ea_xspec: Found more than one arf file of the form ' $
      + arf
    if keyword_set(echo) then print, $
      'ea_xspec: Found more than one arf file of the form ' + arf
    return
  endif

  temp  = "arf " + strn(det+1) + " " + flist(0)
  if det le 1 then s0_1_hdr(5 + det) = temp
  s0_3_hdr(9 + det) = temp

  if (rmg and (det lt 2)) then begin
    rmf = obsname + '_' + detnames(det) + "_" + date + '*rmf'
    flist = ea_fingun(rmf + '*', count=count)
    if count eq 0 then begin
      printf, loglun, 'ea_xspec: Cound not find any rmf files of the form ' $
        + rmf
      if keyword_set(echo) then print, $
        'ea_xspec: Cound not find any rmf files of the form ' + rmf
      return
    endif

    if count ne 1 then begin
      printf, loglun, 'ea_xspec: Found more than one rmf file of the form ' $
        + rmf
      if keyword_set(echo) then print, $
        'ea_xspec: Found more than one rmf file of the form ' + rmf
      return
    endif

    rmf_files(det) = flist(0)
  endif
endfor

if n_elements(z) eq 0 then z = 0.
mo = replicate({xsmod, name:'', modl:strarr(12), n:0}, 5)
mo(0).name = "pl"
mo(0).modl = ['mo cons * wa ( po )', '1.0 -1', '0.1', '1.8', '1e-3']
mo(0).n = 3 ; no. of model params
mo(1).name = "rs"
mo(1).modl = ['mo cons * wa ( ray )',   '1.0 -1', '0.1', '5.0', '0.1 0.001', $
          strn(z), '1e-3']
mo(1).n = 5 ; no. of model params
mo(2).name = "plpl"
mo(2).modl = ['mo constant * wabs( powerlaw + wabs( powerlaw ) )', '1.0 -1', $
              '0.1', '2.5', '1e-3', '1.0', '1.5', '1e-3']
if keyword_set(oldxcm) then mo(2).modl = $
  ['mo constant * wabs( powerlaw + wabs( powerlaw ) )', '1.0 -1', $
   '0.1', '2.5', '1e-3', '1.5', '1e-3', '1.0']

mo(2).n = 6
mo(3).name = "rspl"
mo(3).modl = ['mo constant * wabs( raymond + wabs( powerlaw ) )', '1.0 -1', $
              '0.1', '0.6', '0.1 0.001', strn(z), '1e-3', '1.0', '2.0', '1e-3']
if keyword_set(oldxcm) then $
  mo(3).modl = ['mo constant * wabs( raymond + wabs( powerlaw ) )', '1.0 -1', $
                '0.1', '0.6', '0.1 0.001', strn(z), '1e-3', '2.0', '1e-3', $
               '1.0']
mo(3).n = 8
mo(4).name = "rsrs"
mo(4).modl = ['mo constant * wabs( raymond + wabs ( ray ) )', '1.0 -1', $
   '0.1', '0.6', '0.1 0.001', strn(z), '1e-3', '1.0', '6.0', $
   '0.1 0.001', strn(z), '1e-3']
if keyword_set(oldxcm) then mo(4).modl = $
  ['mo constant * wabs( raymond + wabs ( ray ) )', '1.0 -1', $
   '0.1', '0.6', '0.1 0.001', strn(z), '1e-3', '6.0', $
   '0.1 0.001', strn(z), '1e-3', '1.0']
mo(4).n = 10

ns0_1_hdr = n_elements(s0_1_hdr)
ns0_3_hdr = n_elements(s0_3_hdr) 
tail = ["query y", "fit 200 0.0001", "chat 10", "fit", $
        "fl 0.5 2", "fl 2 10", "", "exit" , "y"]
err = 2 ; location of err command
save = 6 ; location of save all command
ntail = n_elements(tail) 
xcmlist = [" "]
get_lun, lun

for dormg = 0, 1 do begin ; produce xcms for sisrmg rmfs if nec.
  for ndet = 0, 1 do begin ; s0-1 or s0-3
    for modn = 0, 4 do begin ; model no.
      go = 1
      if n_elements(root) gt 0 then fname = root + "_" else $
        fname = obsname + "_"
      if ndet then fname = fname + "s0-3" else fname = fname + "s0-1"
      fname = fname + "_" + mo(modn).name + "_"
; Only produce xcms for all models if requested
      if (xspecflag and 4) eq 4 then go = 1 else go = (modn eq 0)
      if go and dormg then if rmg then fname = fname + "rmg_" else go = 0
      if go and (not dormg) then go = xspecflag and 1
      if go then begin
        fname = fname + date
        tail(save) = "save all " + fname
        fname = fname + "_setup.xcm"
        xcmlist = [xcmlist, fname]
        openw, lun, fname
        if ndet then for i=0, ns0_3_hdr - 1 do printf, lun, s0_3_hdr(i) else $
          for i=0, ns0_1_hdr - 1 do printf, lun, s0_1_hdr(i)
        if dormg then begin
          printf, lun, 'resp 1 ' + rmf_files(0)
          printf, lun, 'resp 2 ' + rmf_files(1)
        endif
        for i=0, mo(modn).n + 1 do printf, lun, mo(modn).modl(i)
        for i=0, ndet*2 do begin ; add padding for S1 (or S1-3)
          printf, lun, '1.0 0.01'
          for j=0, mo(modn).n - 1 do printf, lun, ' = ' + strn(j+2)
        endfor
        for i=0, ntail - 1 do printf, lun, tail(i)
        close, lun
      endif
    endfor
  endfor
endfor

xcmlist = xcmlist(1:*)
if keyword_set(at) then begin
  openw, lun, 'ea_xspec.tmp'
  for i=0, n_elements(xcmlist)-1 do printf, lun, 'xspec ' + xcmlist(i)
  close, lun
  spawn, 'cat ea_xspec.tmp | at now'
  spawn, 'rm ea_xspec.tmp', /sh
endif

free_lun, lun
return
end



