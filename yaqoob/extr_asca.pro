pro extr_asca, obsname, hpcut=hpcut, logfile=logfile, date=date, $
               gradeflt=gradeflt, recover_time=recover_time, aux=aux,$
               sis_xybin=sis_xybin, dosigclean=dosigclean, expmap=expmap, $
               sis_blcbin=sis_blcbin, gis_blcbin = gis_blcbin, spec=spec, $
               arf=arf, binspec=binspec, rmf=rmf, all=all, timeflt=timeflt, $
               sispi=sispi, echo=echo, cleanup=cleanup, xspec=xspec, $
               blank=blank, sisblank=sisblank, gisblank=gisblank, z=z, at=at, $
               doslc=doslc, slc_grp=slc_grp, slc_mint=slc_mint,$
               slc_bin=slc_bin, gisclean=gisclean, prompt=prompt, pspc=pspc, $
               recenter=recenter, sis_reg=sis_reg, stamp=stamp, fixresp=fixresp

if n_elements(sisblank) eq 0 then sisblank = $
  "/FTP/asca/calib_data/sis/bcf/bgd/94nov/"
if n_elements(gisblank) eq 0 then gisblank = $
  "/FTP/asca/calib_data/gis/bcf/bgd/"

if n_params(0) eq 0 then begin
    print, 'extr_asca, obsname, hpcut=hpcut, logfile=logfile, date=date, $'
    print, '          gradeflt=gradeflt, recover_time=recover_time, aux=aux, $'
    print, '          sis_xybin=sis_xybin, dosigclean=dosigclean, expmap=expmap, $'
    print, '          sis_blcbin=sis_blcbin, gis_blcbin = gis_blcbin, spec=spec, $'
    print, '          arf=arf, binspec=binspec, rmf=rmf, all=all, timeflt=timeflt'
    print,'           sispi=sispi, echo=echo, cleanup=cleanup, xspec=xspec, blank=blank $'
    print,'           sisblank=sisblank, gisblank=gisblank, z=z, at=at, $'
    print,'           doslc=doslc, slc_grp=slc_grp, slc_mint=slc_mint,$'
    print,'           slc_bin=slc_bin, gisclean=gisclean, prompt=prompt, pspc=pspc, $
    print,'           recenter=recenter, sis_reg=sis_reg, stamp=stamp, fixresp=fixresp
    print, 'Produce and execute scripts to automatically extract minimal ASCA data
    print, 'products.'
    ans = ''
    print, '$(a, $)', 'Do you want a description of the parameters (y/n)? '
    ans = get_kbrd(1)
    if ans eq 'y' then begin
      print, ''
      print, 'obsname - prefix to apply to all output files, i.e., n3998'
      print, 'logfile - optional log file'
      print, '/echo - echo ouput to log file to screen as well (only nec. if no logfile)'
      print, 'date - for use in creating filename, default = current date in form of 18dec96 
      print, 'hpcut - initial constant counts/pix cutoff, def. = 25'
      print, "gradeflt - grade filter, default = 'grade.ne.1.and.grade.le.4'"
      print, "recover_time - time to apply for t_saa & t_dy_nt for sis mkf cleaning"
      print, "               should be 16 X ccdmode, default = 32 (seconds)"
      print, "sis_xybin - binning used in SIS SAOIMAGE region files, default = 4"
      print, "timeflt - fits GTI file to clean light curves, default is previous
      print, "  output from sigclean
      print, "if /dosigclean, produce fits gti file that would remove points deviating by"
      print, "  more than 3 sigma from the mean in the bgd. light curve"
      print, "  bin bgd. light curve into sis_blcbin or gis_blcbin second bins"
      print, "  Default = 256 secs. for sis_blcbin, 256 secs for gis_blcbin"
      print, "  Overrides value of timeflt"
      print, "if /spec, create and run script to extract spectra"
      print, "if /arf, run ascaarf"
      print, "if binspec is given, bin src. spectra to binspec cts/bin'
      print, "if /rmf, run sisrmg (SIS only, of course)"
      print, "if /expmap, produce exposure maps for the sis obs.'
      print, "$(a, $)", "Press any key to continue "
      print, get_kbrd(1)
      print, "if /xspec, create xspec xcm files for S0-1 and S0-3 pl fits
      print, "  if (xspec and 1), create xcm files using caldb rmfs
      print, "  if (xspec and 2), create xcm files using sisrmg rmfs
      print, "  if (xspec and 4), also create rs, pl+rs, pl+pl, and rs+rs xcm files
      print, "  if (xspec and 8), also create xcm files using blank sky bgds."
      print, "  z optionally gives redshift for rs models
      print, '  /at causes an "at" job to be submitted to run xspec with the xcm files
      print, "if /blank, extract blank sky spectra as an alt. bgd. (not yet implemented"
      print, "  sisblank, gisblank may optionally be set to path to blank sky evts"
      print, "  def. sisblank = " + sisblank
      print, "  def. gisblank = " + gisblank
      print, "if /cleanup, remove temporaries and compress events lists and images"
      print, "if /gisclean, run gisclean on GIS .unf files after mkf cleaning'
      print, "  if evt already exist, then new GIS evt files are created'
      print, "if /ascalin, run ascalin on evt files before extracting spectra'
      print, "if /all, do all of the above (with binspec=20 cts/bin, /xspec)
      print, "if /prompt, prompt before creating source and background regions"
      print, "if pspc contains the filename of a list of pspc event files, then source"
      print, "  and bgd. regions are generated"
      print, "aux gives aux directory, def. = ../aux/"
      print, "if /stamp, creat stamp gifs showing regions"
 endif else print, ''
    retall
endif

if n_elements(hpcut) eq 0 then hpcut = 25
loglun = -1 ; Default behavior is to write to the screen
if n_elements(logfile) ne 0 then openw, loglun, logfile, /get_lun
if n_elements(aux) eq 0 then aux = '../aux/'
printf, loglun, systime()

if n_elements(date) eq 0 then begin
  date =  strmid(systime(), 8, 2) + strmid(systime(), 4, 3) + $
  strmid(systime(),22, 2)
  date = strlowcase(date)
  if strmid(date, 0, 1) eq ' ' then strput, date, '0', 0
endif

if n_elements(gradeflt) eq 0 then gradeflt = "grade.ne.1.and.grade.le.4"
if n_elements(recover_time) eq 0 then recover_time = 32.
if n_elements(sis_xybin) eq 0 then sis_xybin = 4
if n_elements(sis_blcbin) eq 0 then sis_blcbin = 256
if n_elements(gis_blcbin) eq 0 then gis_blcbin = 256
dopspc = n_elements(pspc) eq 1
if dopspc then begin
  flist = findfile(pspc, count=count)
  if count eq 0 then begin
    printf, loglun, 'Could not find ' + pspc
    if keyword_set(echo) then print, 'Could not find ' + pspc 
  endif
  dopspc = count eq 1
endif

if keyword_set(all) then begin
  binspec = 20
  arf = 1
  spec = 1
  rmf = 1
  if n_elements(dosigclean) eq 0 then dosigclean = 3
  expmap = 1
  blank = 1
  if n_elements(xspec) eq 0 then xspec = 1
  doslc = 1
  cleanup = 1
  gisclean = 1
  stamp = 1
endif

if n_elements(slc_bin) eq 0 then slc_bin = 4000
if n_elements(slc_grp) eq 0 then slc_grp = 20
if n_elements(slc_mint) eq 0 then slc_mint = 100
if n_elements(binspec) ne 0 or keyword_set(arf) or keyword_set(rmf) or $
  keyword_set(blank) then spec = 1
   
; Is this an interactive session?  Interactive session almost always
; identify themselves as vt100, vt200, etc.
;interflag = strmid(getenv("TERM"), 0, 2) eq "vt"
interflag = keyword_set(prompt) and (strmid(getenv("TERM"), 0, 2) eq "vt")

nfiles = intarr(4, 3) ; (det, bit_rate - 0=low, 1=med, 2=high)
rawiname = ["S0", "S1", "G2", "G3"]
iname = ["s0", "s1", "g2", "g3"]
bitrate = ["L", "M", "H"]

;if keyword_set(gisclean)then print, '/gisclean is set'
; Search for raw files
for i=0,3 do begin
  for j=0, 2 do begin
    root = "*" + rawiname(i)
    if i < 2 then root = root + "*02" else root = root + "*"
    flist = findfile(root + bitrate(j) + ".fits*", count=count)
    nfiles(i, j) = count
; Check to see if files need to be decompressed
;    ea_gunzip, flist
  endfor
  tot = fix(total(nfiles(i, *)))

; Do initial processing with all bitrates that are present
  if tot gt 0 then begin
    printf, loglun, 'Found ' + strn(tot) + " " + rawiname(i) + " files"
    if keyword_set(echo) then print, 'Found ' + strn(tot) + " " + rawiname(i) + " files"
    bitrate_list = [-1]
    for j=0, 2 do begin
      printf, loglun, strn(nfiles(i, j)) + " (" + $
        strn(100*nfiles(i, j)/tot) + "%) " + bitrate(j)
      if keyword_set(echo) then print, strn(nfiles(i, j)) + " (" + $
        strn(100*nfiles(i, j)/tot) + "%) " + bitrate(j)
      if nfiles(i, j) gt 0 then bitrate_list = [bitrate_list, j]
    endfor
    ea_mergeraw, obsname, i, bitrate_list(1:*), date=date, loglun=loglun, $
      echo=echo
  endif else begin
    printf, loglun, "No " + rawiname(i) + ' "raw" files found'
    if keyword_set(echo) then print, "No " + rawiname(i) + ' "raw" files found' 
  endelse
  flush, loglun
endfor

; Search for unf files
for i=0,3 do begin
  unfsrc = "*" + iname(i) + "*unf*"
  flist = findfile(unfsrc, count=count)
;  unfsrc = "*" + iname(i) + "*unf"
 
  if count gt 0 then begin
    printf, loglun, 'Found ' + strn(count) + " " + iname(i) + " unf files"
    if keyword_set(echo) then print, 'Found ' + strn(count) + " " + iname(i) $
      + " unf files"
    xybin = 1
    if i lt 2 then begin
      xybin = sis_xybin
      if keyword_set(sispi) then begin
        flist = ea_fingun(unfsrc, count=count)
        for j=0, count-1 do begin
          sispi_str = "sispi " + flist(j) + " FTOOLS"
          printf, loglun, sispi_str
          if keyword_set(echo) then print, sispi_str
          spawn, sispi_str, result, count=rcount
          for k=0, rcount-1 do printf, loglun, result(k)
        endfor
      endif
    endif
    ea_init, obsname, i, hpcut, gradeflt, gisclean=gisclean, aux=aux, $
      recover_time, xybin=xybin, loglun=loglun, date=date, echo=echo
;    if i eq 3 then gisclean = 0
  endif else begin
    printf, loglun, "No " + iname(i) + ' unf files found'
    if keyword_set(echo) then print, "No " + iname(i) + ' unf files found' 
  endelse
  flush, loglun
endfor

; Search for screened files
for i=0, 3 do begin
  flist = findfile("ad*" + iname(i) + "*evt*", count=count)
 
  if count gt 0 then begin
    printf, loglun, 'Found ' + strn(count) + " " + iname(i) + $
      ' "screened" evt files'
    if keyword_set(echo) then print, 'Found ' + strn(count) + " " + iname(i) $
      + ' "screened" evt files'
    ea_mergescr, obsname, i, loglun=loglun, date=date, echo=echo
  endif else begin
    printf, loglun, "No " + iname(i) + ' "screened" evt files found'
    flush, loglun
    if keyword_set(echo) then print, "No " + iname(i) + ' "screened" evt files found' 
  endelse
endfor

; if it appears the gisclean is still "set" then /gisclean was specified but
; ea_init.pro had been run previously... in this case create new *mkf.evt files
; for GIS
gis_cl = ""
if keyword_set(gisclean) then begin
;  print, '/gisclean is still set'
  giscl = "_gis_cl"
  ea_giscl, obsname, loglun=loglun, date=date, echo=echo
endif
 
; Okay, now we have events lists with preliminary cleaning done
; if region files exist, then we can proceed
for i=0, 3 do begin
  printf, loglun, ' '
  printf, loglun, string(replicate(45b,79))
  printf, loglun, iname(i)
  printf, loglun, string(replicate(45b,79))
  if keyword_set(echo) then begin
    print, ' '
    print, string(replicate(45b,79))
    print, iname(i)
    print, string(replicate(45b,79))
  endif 

; Generate root name of product files and see if we need to make src/bgd reg
  got_srcbgd = 0
  root = obsname + "_" + strlowcase(iname(i)) + "_"
  srcreg = root + "src.reg"
  bgdreg = root + "bgd.reg"
  root = root + date
  if i ge 2 then root = root + gis_cl
  evtfil = root + "_mkf.evt"
  flist = findfile(evtfil + "*", count=count)
  xybin = 1
  if i lt 2 then xybin = sis_xybin
  if count eq 1 then begin
    flist = findfile(srcreg, count=count)
    if count eq 1 then begin
      flist = findfile(bgdreg, count=count)
      if count eq 1 then begin
        ea_getsrcbgd, evtfil, srcreg, bgdreg, xybin=xybin, loglun=loglun, echo=echo
        got_srcbgd = 1
      endif else begin
        printf, loglun, 'Could not find ' + bgdreg
        if keyword_set(echo) then print, 'Could not find ' + bgdreg 
      endelse
    endif else begin
      printf, loglun, 'Could not find ' + srcreg
      if keyword_set(echo) then print, 'Could not find ' + srcreg  
    endelse
; Try to generate source and background regions automatically
    if not got_srcbgd then begin
      get_reg = 1
      if interflag then begin
        ans = ''
        read, 'Generate source and background regions (y/n)? ', ans
        get_reg = ans eq 'y'
      endif
      if get_reg then begin
        ea_genreg, evtfil, srcreg, bgdreg, xybin=xybin, loglun=loglun, $
          echo=echo, sis_reg=sis_reg
        ea_getsrcbgd, evtfil, srcreg, bgdreg, xybin=xybin, loglun=loglun, echo=echo
        got_srcbgd = 1
      endif
    endif

    if not got_srcbgd then begin
      printf, loglun, 'Cannot proceed without source and background regions'
      if keyword_set(echo) then print, 'Cannot proceed without source and background regions'
      if loglun ne -1 then free_lun, loglun
      return
    endif

    flush, loglun

; Clean bgd. lc, possible
    if n_elements(timeflt) eq 0 then begin
      temp = root + '_mkf_bgd.gti*'
      flist = ea_fingun(temp, count=count)
      if count eq 1 then timeflt = flist(0) else timeflt = ''
    endif else count=1

    flist = ea_fingun(root + '_mkf_bgd.gti*', count=count)
    if got_srcbgd and keyword_set(dosigclean) and count gt 0 then begin
      printf, loglun, 'A gti file already exists... delete it if you want to run sigclean'
      if keyword_set(echo) then print, 'A gti file already exists... delete it if you want to ru sigclean'
    endif
    if got_srcbgd and keyword_set(dosigclean) and count eq 0 then begin
      if i lt 2 then blcbin = sis_blcbin else blcbin = gis_blcbin
      flist = ea_fingun(root + "_mkf_bgd.evt*", count=count)
      if count gt 0 then begin
        qreadasca, root + "_mkf_bgd.evt", plist, h, gti
        tin0 = total(gti(*, 1) - gti(*, 0)) 
        for nsigcl = 1, dosigclean do begin
          tin = total(gti(*, 1) - gti(*, 0))
          printf, loglun, "Cleaning bgd. lc, initial exp. time = " + $
            strn(tin) + ' secs.'
          if keyword_set(echo) then print, "Cleaning bgd. lc, initial exp. time = " + $
            strn(tin) + ' secs.'
          timeflt = root + '_mkf_bgd.gti'
          sigclean, plist, gti, clgti, bin=blcbin, /make_qdp, qdpin=root + $
            "_mkf_bgd_in" + strn(blcbin) + "_i" + strn(nsigcl) + ".qdp", $
            qdpout=root + "_mkf_bgd_out" + strn(blcbin) + "_i" + strn(nsigcl) $
            + ".qdp", fits=timeflt, loglun=loglun
          tout = total(clgti(*, 1) - clgti(*, 0))
          printf, loglun, 'sigclean iteration ' + strn(nsigcl) + $
            ' removed a total of ' + strn(tin-tout) + ' secs.'
          if keyword_set(echo) then print, 'sigclean iteration ' + strn(nsigcl) + $
            ' removed a total of ' + strn(tin-tout) + ' secs.'
          gti = clgti
          gtiflt, plist, gti, /silent
        endfor
          printf, loglun, 'Saving final gti in ' + timeflt + ' with exp. time ' $
            + strn(tout) + ' secs., a decrease of ' + strn(tin0 - tout) + ' secs.'
          if keyword_set(echo) then print, 'Saving final gti in ' + timeflt + ' with exp. time ' $
            + strn(tout) + ' secs., a decrease of ' + strn(tin0 - tout) + ' secs.'
          flush, loglun
      endif else begin
        printf, loglun, "Counld not find " + root + "_mkf_bgd.evt"
        if keyword_set(echo) then print, "Counld not find " + root + "_mkf_bgd.evt"
      endelse
    endif
    
    if keyword_set(spec) then begin
      flist = ea_fingun(evtfil + '*')
      ea_spec, evtfil, i, srcreg, bgdreg, rmf=rmf, echo=echo, $
        xybin=xybin, timeflt=timeflt, arf=arf, rebin=binspec, loglun=loglun, $
        blank=blank
      flush, loglun
    endif

    if keyword_set(expmap) then ea_expmap, root, i, timeflt, $
      loglun=loglun, date=date, echo=echo, aux=aux
 
  endif else begin
    printf, loglun, 'Could not find ' + root + '* files' 
    if keyword_set(echo) then print, 'Could not find ' + root + '* files' 
  endelse
endfor

if keyword_set(stamp) then begin
;  if keyword_set(gisclean) then print, '/gisclean is still set'
  ea_stamp, obsname, loglun=loglun, date=date, echo=echo, sis_xybin=sis_xybin,$
    gis_xybin=gis_xybin, gisclean=gisclean
endif

if keyword_set(xspec) then begin
  ea_xspec, xspec, obsname, z=z, loglun=loglun, date=date, echo=echo, at=at
endif

if keyword_set(doslc) then begin
  ea_lc, obsname, date=date, loglun=loglun, echo=echo, grpmin=slc_grp, $
    bin=slc_bin, mint=slc_mint, gisclean=gisclean
endif

if dopspc then begin
  ea_pspc, pspc, obsname, date=date, loglun=loglun, echo=echo, $
    sis_xybin=sis_xybin, gis_xybin=gis_xybin, recenter=recenter
endif

if keyword_set(cleanup) then begin
  spawn, 'rm -e *exp', /sh
  spawn, 'rm -e xsel*', /sh
  spawn, 'gzip -f -v *img *fits *unf *evt'
  spawn, 'gzip -f -v ' + aux + '*fits ' + aux + '*mkf'
endif

printf, loglun, systime()
if loglun gt 0 then free_lun, loglun
return
end
