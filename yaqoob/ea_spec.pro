pro ea_spec, evtfil, det, srcreg, bgdreg, xybin=xybin, timeflt=timeflt,$
  arf=arf, rebin=rebin, loglun=loglun, rmf=rmf, echo=echo, $
  blank=blank, sisblank=sisblank, gisblank=gisblank

if n_params(0) eq 0 then begin
  print, 'ea_spec, evtfil, det, srcreg, bgdreg, xybin=xybin, timeflt=timeflt, $'
  print, '  arf=arf, rebin=rebin, loglun=loglun, rmf=rmf, echo=echo, $
  print, '  blank=blank, sisblank=sisblank, gisblank=gisblank'
  print, 'Creates an xselect script to extract src and bgd spectra from evtfil'
  print, 'Intended to be called from extr_asca'
  retall
endif

if n_elements(xybin) eq 0 then xybin = 1
if n_elements(loglun) eq 0 then loglun = -1 

dot = strpos(evtfil, '.')
if dot lt 0 then stop
srcspec = strmid(evtfil, 0, dot) + '_src.pha'
bgdspec = strmid(evtfil, 0, dot) + '_bgd.pha'
respfiles = [$
  '/FTP/caldb/data/asca/sis/cpf/94nov9/s0c1g0234p40e1_512v0_8i.rmf', $
  '/FTP/caldb/data/asca/sis/cpf/94nov9/s1c3g0234p40e1_512v0_8i.rmf', $
  '/FTP/caldb/data/asca/gis/cpf/95mar06/gis2v4_0.rmf', $
  '/FTP/caldb/data/asca/gis/cpf/95mar06/gis3v4_0.rmf']

sname = strmid(evtfil, 0, dot) + '_spec.xco'
flist = findfile(sname, count=count)
if count gt 0 then begin
  printf, loglun, sname + ' already exists, delete it if you want '
  printf, loglun, 'the source and background spectra to be re-extracted'
  if keyword_set(echo)  then begin
    print, sname + ' already exists, delete it if you want '
    print, 'the source and background spectra to be re-extracted'
  endif
endif else begin

  openw, lun, sname, /get_lun

  if det lt 2 then group = " group=yes" else group = " group=no"
  printf, lun, "xsel"
  printf, lun, "read eve " + evtfil + " ."
  printf, lun, "set xybin " + strn(xybin)
  if n_elements(timeflt) gt 0 then if timeflt ne '' then printf, lun, $
    "filter time file " + timeflt 
  printf, lun, "filter region " + srcreg
  printf, lun, "extr spec"
  printf, lun, "save spec " + srcspec + group + " clobber=yes"
  printf, lun, "clear events"
  printf, lun, "clear region"
  printf, lun, "filter region " + bgdreg
  printf, lun, "extr spec"
  printf, lun, "save spec " + bgdspec + group + " clobber=yes"
  printf, lun, "exit"
  printf, lun, "no"
  free_lun, lun
  printf, loglun, 'xselect @' + sname
  flush, loglun
  if keyword_set(echo) then print, 'xselect @' + sname 
  spawn, 'xselect @' + sname, result, count=count
  for i=0, count-1 do printf, loglun, result(i)
  if keyword_set(echo) then for i=0, count-1 do print, result(i)
endelse

if keyword_set(arf) then begin
  arf_str = 'ascaarf ' + srcspec + ' ' + respfiles(det) + ' ' + $
    strmid(evtfil, 0, dot) + '_src.arf yes yes'
  printf, loglun, arf_str
  flush, loglun
  if keyword_set(echo) then print, arf_str 
  spawn, arf_str, result, count=count
  for i=0, count-1 do printf, loglun, result(i)
  if keyword_set(echo) then for i=0, count-1 do print, result(i)
endif

if n_elements(rebin) gt 0 then begin
  if det lt 2 then bad = 'bad 1-13 343-512' else bad = 'bad 0-69 850-1023'
  outfil = strmid(evtfil, 0, dot) + '_src_b' + strn(rebin) + '.pha '
  printf, loglun, 'rm '  + outfil
  if keyword_set(echo) then print, 'rm '  + outfil
  spawn, 'rm ' + outfil, /sh
  grp_str = 'grppha ' + srcspec + ' ' + outfil + 'comm = "'+ bad + $
    ' & group min ' + strn(rebin) + ' & chkey resp ' + respfiles(det) + $
    ' & exit "'
  printf, loglun, grp_str
  if keyword_set(echo) then print, grp_str
  flush, loglun
  spawn, grp_str
endif

if det lt 2 and keyword_set(rmf) then begin
  spawn, 'rm  ' + strmid(evtfil, 0, dot) + '_src.rmf', /sh
  printf, loglun, 'rm  ' + strmid(evtfil, 0, dot) + '_src.rmf'
  if keyword_set(echo) then print, 'rm  ' + strmid(evtfil, 0, dot) + '_src.rmf'
  rmf_str = 'sisrmg ' + srcspec + ' NONE ' + $
    strmid(evtfil, 0, dot) + '_src.rmf'
  printf, loglun, rmf_str
  if keyword_set(echo) then print, rmf_str
  spawn, rmf_str
endif

return
end
