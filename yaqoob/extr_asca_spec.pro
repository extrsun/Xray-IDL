pro extr_asca_spec, evtfil, det, srcreg, bgdreg, xybin=xybin, timeflt=timeflt,$
  arf=arf, rebin=rebin, loglun=loglun, rmf=rmf, sisfile=sisfile, grdstr=grdstr, sisgain=sisgain, ptsrc=ptsrc, bugcor=bugcor, sisdatadir=sisdatadir  

if n_params(0) eq 0 then begin
  print, 'extr_asca_spec, evtfil, det, srcreg, bgdreg, xybin=xybin, timeflt=timeflt, $'
  print, '  arf=arf, rebin=rebin, loglun=loglun, rmf=rmf, sisfile=sisfile, grdstr=grdstr,sisgain=sisgain, ptsrc=ptsrc, bugcor=bugcor, sisdatadir=sisdatadir'
  print, 'Creates an xselect script to extract src and bgd spectra from evtfil'
  print, 'Intended to be called from extr_asca'
  print,'SISGAIN = array(2) if specified, the gain is increased by this '
  print,'percentage by dividing nom_gain by 1.+(sisgain/100.)'
  print,'PTSRC	- yes or no POINT source for ASCAARF: default yes'
  print,'BUGCOR - run fixsisrmg - default NO'
  print,'sisdatadir - refdata directory containing sisrmg cal files '
  print,'   	    - default is /local/home/rohrshni/yaqoob/siscal/'
  retall
endif

if n_elements(xybin) eq 0 then xybin = 1
if n_elements(loglun) eq 0 then loglun = -1 
if n_elements(rmf) eq 0 then rmf =0 
if n_elements(ptsrc) eq 0 then ptsrc='yes'
if n_elements(bugcor) eq 0 then bugcor='no' 
if n_elements(sisdatadir) eq 0 then sisdatadir='/local/home/rohrshni/yaqoob/siscal/'
if n_elements(sisfile) eq 0 then sisfile='/local/home/rohrshni/yaqoob/siscal/sisph2pi_110397.fits'
dot = strpos(evtfil, '_all.')
if dot lt 0 then dot = strpos(evtfil, '.') 
srcspec = strmid(evtfil, 0, dot) + '_src.pha'
srceve = strmid(evtfil, 0, dot) + '_src.evt'
bgdspec = strmid(evtfil, 0, dot) + '_bgd.pha'
bgdeve = strmid(evtfil, 0, dot) + '_bgd.evt'
bbit='_src_b'
rbit='_src'
if n_elements(sisgain) gt 0 and det le 1 then begin
 strput,srcspec,'g',strlen(srcspec)-8
 strput,srceve,'g',strlen(srcspec)-8
 strput,bgdspec,'g',strlen(srcspec)-8
 strput,bgdeve,'g',strlen(srcspec)-8
 bbit='_src_g' & rbit='_srcg' 
endif
ss=strpos(sisfile,'sisph2pi')
phpifile=strmid(sisfile,ss,20)
respfiles = [$
  '/FTP/caldb/data/asca/sis/cpf/94nov9/s0c1g0234p40e1_512v0_8i.rmf', $
  '/FTP/caldb/data/asca/sis/cpf/94nov9/s1c3g0234p40e1_512v0_8i.rmf', $
  '/FTP/caldb/data/asca/gis/cpf/95mar06/gis2v4_0.rmf', $
  '/FTP/caldb/data/asca/gis/cpf/95mar06/gis3v4_0.rmf']

if det le 1 and n_elements(sisgain) gt 0 then begin
 if n_elements(sisfile) eq 0 then sisfile=' FTOOLS '
 ssh=headfits(sisfile,ext=1) & nom_gain=sxpar(ssh,'nom_gain')
 newgain=nom_gain/(1.+(sisgain(det)/100.))
 sispi_str = "sispi " + " datafile = "+evtfil+" calfile = "+sisfile+" gainnom = "+strtrim(newgain,2)
 spawn,sispi_str,/sh
endif
sname = strmid(evtfil, 0, dot) + '_spec.xco'
flist = findfile(sname, count=count)
;if count gt 0 then begin
  printf, loglun, sname + ' already exists, overwriting'
;delete it if you want the source'
;  printf, loglun, 'and background spectra to be re-extracted'
;endif else begin

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
  printf, lun, "extr events"
  printf, lun, "save events " + srceve + " clobber=yes"
  printf, lun, "no"
  printf, lun, "clear events"
  printf, lun, "clear region all"
  printf, lun, "filter region " + bgdreg
  printf, lun, "extr spec"
  printf, lun, "save spec " + bgdspec + group + " clobber=yes"
  printf, lun, "extr events"
  printf, lun, "save events " + bgdeve + " clobber=yes"
  printf, lun, "no"
  printf, lun, "clear events"
  printf, lun, "clear region all"
  printf, lun, "exit"
  printf, lun, "no"
  free_lun, lun
  print,'written xselect file now running xselect '
  xselstr='xselect @'+sname
  spawn, xselstr, result, count=count,/sh
  for i=0, count-1 do printf, loglun, result(i)
;endelse
print,'ran xselect ...'

arfname='none'
respname=respfiles(det)
;use simple=no and read the source cetroid from the region
;file
openr,9,srcreg
dummy=' '
rdf: readf,9,dummy
 if strmid(dummy,0,1) eq '#' then goto, rdf
 pstr1=str_sep(dummy,'(')  & reads,pstr1(1),xc,yc,rs
close,9 
if keyword_set(arf) then begin
  arf_str = 'ascaarf ' + srcspec + ' ' + respfiles(det) + ' ' + $
    strmid(evtfil, 0, dot) + '_src.arf '+strtrim(ptsrc,2)+' no '+ $
    strtrim(strn(xc),2)+' '+strtrim(strn(yc),2)
  printf, loglun, arf_str
  flush, loglun
  spawn, arf_str, result, count=count,/sh
  for i=0, count-1 do printf, loglun, result(i)
  arfname=strmid(evtfil, 0, dot)+'_src.arf'
endif
if det lt 2 and rmf gt 0 then begin
;first correct for bug in SISRMG FTOOLS v3.6
 if bugcor eq 'yes' then fixsisrmg,srcspec,evname=srceve,sisfile=sisfile,$
 /batch,oldevt=oldevt,oldlvdl=oldlvdl,kname1=kname1,kname2=kname2 
  spawn, 'rm  ' + strmid(evtfil, 0, dot) + rbit+'.rmf', /sh
  printf, loglun, 'rm  ' + strmid(evtfil, 0, dot) + rbit+'.rmf'
  rmf_str = 'sisrmg ' + srcspec + ' NONE ' + $
    strmid(evtfil, 0, dot) + rbit+'.rmf'+' phtopi= '+phpifile+' grades = '+$
 strtrim(grdstr,2)+' datadir = '+strtrim(sisdatadir,2)
  printf, loglun, rmf_str
  spawn, rmf_str,/sh
  respname=strmid(evtfil, 0, dot)+rbit+'.rmf'
;made the response file - if bugcor then put back original keywords
 if bugcor eq 'yes' then begin
  pbstr='fparkey value = '+strtrim(string(oldevt),2)+' fitsfile = '+strtrim(srcspec,2)+'+0 keyword = '+kname1 
  spawn, pbstr, /sh
  print,'Inserted back ',kname1, ' = ',oldevt
  pbstr='fparkey value = '+strtrim(string(oldlvdl),2)+' fitsfile = '+strtrim(srcspec,2)+'+0 keyword = '+kname2 
  spawn, pbstr, /sh
  print,'Inserted back ',kname2, ' = ',oldlvdl
 endif
endif
bgdfile=bgdspec
;bgdfile=strmid(evtfil, 0, dot) + '_bgd.pha'
if n_elements(rebin) gt 0 then begin
  if rmf eq 0 then respname=strmid(evtfil, 0, dot)+'_src.rmf'
  arfname=strmid(evtfil, 0, dot)+'_src.arf'
  if det eq 0 then bad = 'bad 0-15 361-512' 
  if det eq 1 then bad = 'bad 0-15 386-512' 
  if det ge 2 then bad = 'bad 0-69 850-1024'
  outfil = strmid(evtfil, 0, dot) + bbit + strn(rebin) + '.pha '
  printf, loglun, 'rm '  + outfil
  spawn, 'rm ' + outfil, /sh
  grp_str = 'grppha ' + srcspec + ' ' + outfil + ' comm = " '+ bad + $
    ' & group min ' + strn(rebin) + ' & chkey resp ' + respname + $
    ' & chkey ancrfile ' + arfname + ' & chkey backfile '$
    +strtrim(bgdfile,2) + ' & exit " '
  printf, loglun, grp_str
  flush, loglun
  spawn, grp_str,/sh
endif

return
end
