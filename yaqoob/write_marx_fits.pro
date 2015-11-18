pro write_marx_fits, outfil, plist, gti

if n_params(0) eq 0 then begin
  print, 'write_marx_fits, outfil, plist, gti'
  print, 'Writes out a fits events file with plist and gti similar to those'
  print, 'produced by marx2fits'
  retall
endif

mission = "AXAF"
telesc = "AXAF"
instr = "ACIS"
obs_id = '0'
object = "UNKNOWN"
type = strmid(tag_names(plist,/struct), 0, 4)
if type eq 'ACIS' then grating = 'NONE' else grating = type
date_obs = '15/01/98'
time_obs = '03:08:29'
date_end = '06/01/98'
time_end = '06:15:09'

neve = n_elements(plist)

; Make primary header
fxhmake, header, /extend, /date
fxaddpar, header, 'CONTENT', 'BASIC'
fxaddpar, header, 'mission', mission
fxaddpar, header, 'telescop', telesc
fxaddpar, header, 'instrume', instr
fxaddpar, header, 'object', object
fxaddpar, header, 'title', object
fxaddpar, header, 'obs_id', obs_id
fxaddpar, header, 'grating', grating
fxaddpar, header, 'observer', getenv('USER')
fxaddpar, header, 'obs_mode', 'pointing'
fxaddpar, header, 'readmode', 'timed'
fxaddpar, header, 'sci_mode', 'photon_count'
fxaddpar, header, 'instmode', 'fpi'
fxaddpar, header, 'ra_nom', 0
fxaddpar, header, 'dec_nom', 0
fxaddpar, header, 'roll_nom', 0
fxaddpar, header, 'DATE-OBS', date_obs ; taken from gal_cxb_60min.fits
fxaddpar, header, 'TIME-OBS', time_obs ; taken from gal_cxb_60min.fits
fxaddpar, header, 'DATE-END', date_end ; taken from gal_cxb_60min.fits
fxaddpar, header, 'TIME-END', time_end ; taken from gal_cxb_60min.fits
fxaddpar, header, 'mjd-obs', 50828.6 ; taken from gal_cxb_60min.fits
fxaddpar, header, 'bias_dat', 'Unknown'
fxaddpar, header, 'bias_tim', 'Unknown'
fxaddpar, header, 'creator', 'write_marx_fits.pro'
fxaddpar, header, 'ascver', 'ACIS_L0_2.0' ; taken from gal_cxb_60min.fits
fxaddpar, header, 'origin', 'cmu'
fxaddpar, header, 'revision', 0
fxaddpar, header, 'event', 'events'
fxaddpar, header, 'dataclas', 'simulated'
fxaddpar, header, 'mjdref', 49353.7
fxaddpar, header, 'tstart', min(gti)
fxaddpar, header, 'tstop', max(gti)
fxaddpar, header, 'telapse', max(gti) - min(gti)
fxaddpar, header, 'NEVENTS', neve
fxaddpar, header, 'timeunit', 's'
fxaddpar, header, 'timesys', 1994.0
fxaddpar, header, 'timedel', 0.2
fxaddpar, header, 'timeref', 'LOCAL'
fxaddpar, header, 'timversn', 'OGIP/93-003a'
fxaddpar, header, 'tassign', 'SATELLITE'

fxwrite,outfil,header

print, 'Writing out gti...'
ngti = (size(gti))(1)
fxbhmake, h1, ngti, 'STDGTI'
fxbaddcol, startcol, h1, gti(0,0), 'START'
fxbaddcol, stopcol, h1, gti(0,0), 'STOP'
fxaddpar, h1, 'cnc1', 2
fxaddpar, h1, 'cetyp1', 'R'
fxaddpar, h1, 'cityp1', '[)'
fxaddpar, h1, 'tdisp1', 'F20.6'
fxaddpar, h1, 'tdisp2', 'F20.6'
fxbcreate, unit, outfil, h1
for i=0l, ngti-1 do begin
  fxbwrite, unit, gti(i, 0), startcol, i+1
  fxbwrite, unit, gti(i, 1), stopcol, i+1
endfor
fxbfinish, unit

fxbhmake, h2, double(neve), 'EVENTS'
fxbaddcol, tcol, h2, plist(0).time, 'TIME'
fxbaddcol, expnocol, h2, -1, 'EXPNO'
fxbaddcol, chipxcol, h2, plist(0).chipx, 'CHIPX'
fxbaddcol, chipycol, h2, plist(0).chipy, 'CHIPY'
fxbaddcol, detxcol, h2, plist(0).detx, 'DETX'
fxbaddcol, detycol, h2, plist(0).dety, 'DETY'
fxbaddcol, tdetxcol, h2, plist(0).tdetx, 'TDETX'
fxbaddcol, tdetycol, h2, plist(0).tdety, 'TDETY'
fxbaddcol, xcol, h2, -1, 'X'
fxbaddcol, ycol, h2, -1, 'Y'
fxbaddcol, rawphacol, h2, plist(0).raw_phas, 'RAW_PHAS'
fxbaddcol, ccdidcol, h2, plist(0).ccdid, 'CCDID'
fxbaddcol, phacol, h2, plist(0).pha, 'PHA'
fxbaddcol, gradecol, h2, plist(0).grade, 'GRADE'
fxbaddcol, fltgradecol, h2, plist(0).fltgrade, 'FLTGRADE'
fxbaddcol, shellcol, h2, -1, 'SHELL'
if type ne 'ACIS' then fxbaddcol, ordercol, h2, plist(0).order, 'ORDER'
fxbaddcol, zcoscol, h2, -1., 'ZCOS'
fxbaddcol, ycoscol, h2, -1., 'YCOS'
fxbaddcol, xcoscol, h2, -1., 'XCOS'
fxbaddcol, zposcol, h2, plist(0).zpos, 'ZPOS'
fxbaddcol, yposcol, h2, plist(0).ypos, 'YPOS'
fxbaddcol, xposcol, h2, -1., 'XPOS'
fxbaddcol, ecol, h2, plist(0).energy, 'ENERGY'
fxaddpar, h2, 'mission', mission
fxaddpar, h2, 'telescop', telesc
fxaddpar, h2, 'instrume', instr
fxaddpar, h2, 'object', object
fxaddpar, h2, 'title', object
fxaddpar, h2, 'obs_id', obs_id
fxaddpar, h2, 'grating', grating
fxaddpar, h2, 'observer', getenv('USER')
fxaddpar, h2, 'obs_mode', 'pointing'
fxaddpar, h2, 'readmode', 'timed'
fxaddpar, h2, 'datamode', 'GRADED'
fxaddpar, h2, 'sci_mode', 'photon_count'
fxaddpar, h2, 'instmode', 'fpi'
fxaddpar, h2, 'ra_nom', 0
fxaddpar, h2, 'dec_nom', 0
fxaddpar, h2, 'roll_nom', 0
fxaddpar, h2, 'DATE-OBS', date_obs ; taken from gal_cxb_60min.fits
fxaddpar, h2, 'TIME-OBS', time_obs ; taken from gal_cxb_60min.fits
fxaddpar, h2, 'DATE-END', date_end ; taken from gal_cxb_60min.fits
fxaddpar, h2, 'TIME-END', time_end ; taken from gal_cxb_60min.fits
fxaddpar, h2, 'mjd-obs', 50828.6 ; taken from gal_cxb_60min.fits
fxaddpar, h2, 'danam2', 'exposure'
fxaddpar, h2, 'dauni2', 's'
fxaddpar, h2, 'hduclass', 'ogip'
fxaddpar, h2, 'hduclas1', 'events'
fxaddpar, h2, 'origin', 'CMU'
fxaddpar, h2, 'grd_schm', 'ACIS 1.0'
fxaddpar, h2, 'evtimode', 9
fxaddpar, h2, 'mjdref', 49353.7
fxaddpar, h2, 'tstart', min(gti)
fxaddpar, h2, 'tstop', max(gti)
fxaddpar, h2, 'telapse', max(gti) - min(gti)
fxaddpar, h2, 'NEVENTS', neve
fxaddpar, h2, 'timeunit', 's'
fxaddpar, h2, 'timesys', 1994.0
fxaddpar, h2, 'timedel', 0.2
fxaddpar, h2, 'timeref', 'LOCAL'
fxaddpar, h2, 'timversn', 'OGIP/93-003a'
fxaddpar, h2, 'tassign', 'SATELLITE'
fxaddpar, h2, 'ACSYS4', 'AXAF-ACIS-1.0'
fxaddpar, h2, 'ACSYS5', 'AXAF-ACIS-1.0'
fxaddpar, h2, 'prefx1', 'CHIPX'
fxaddpar, h2, 'prefx2', 'CHIPY'
fxaddpar, h2, 'TLMIN' + strn(chipxcol), 0
fxaddpar, h2, 'TLMAX' + strn(chipxcol), 1023
fxaddpar, h2, 'TLMIN' + strn(chipycol), 0
fxaddpar, h2, 'TLMAX' + strn(chipycol), 1023
;fxaddpar, h2, 'TLMIN' + strn(phacol), min(plist.pha)
;fxaddpar, h2, 'TLMAX' + strn(phacol), max(plist.pha)
fxaddpar, h2, 'TLMIN' + strn(phacol), 0
fxaddpar, h2, 'TLMAX' + strn(phacol), 4095
fxaddpar, h2, 'TLMIN' + strn(yposcol), min(plist.ypos)
fxaddpar, h2, 'TLMAX' + strn(yposcol), max(plist.ypos)
fxaddpar, h2, 'TLMIN' + strn(zposcol), min(plist.zpos)
fxaddpar, h2, 'TLMAX' + strn(zposcol), max(plist.zpos)
fxaddpar, h2, 'TLMIN' + strn(ecol), min(plist.energy)
fxaddpar, h2, 'TLMAX' + strn(ecol), max(plist.energy)
fxaddpar, h2, 'TLMIN' + strn(tdetxcol), 1
fxaddpar, h2, 'TLMIN' + strn(tdetycol), 1
fxaddpar, h2, 'TLMIN' + strn(detxcol), 1
fxaddpar, h2, 'TLMIN' + strn(detycol), 1
fxaddpar, h2, 'TLMIN' + strn(xcol), 1
fxaddpar, h2, 'TLMIN' + strn(ycol), 1
if type ne 'ACIS' then begin
    fxaddpar, h2, 'TLMAX' + strn(tdetxcol), 4096
    fxaddpar, h2, 'TLMAX' + strn(tdetycol), 4096
    fxaddpar, h2, 'TLMAX' + strn(detxcol), 4096
    fxaddpar, h2, 'TLMAX' + strn(detycol), 4096
    fxaddpar, h2, 'TLMAX' + strn(xcol), 4096
    fxaddpar, h2, 'TLMAX' + strn(ycol), 4096
    fxaddpar, h2, 'DET_XSIZ', 5200
    fxaddpar, h2, 'DET_YSIZ', 5200
endif else begin
    fxaddpar, h2, 'TLMAX' + strn(tdetxcol), 6144
    fxaddpar, h2, 'TLMAX' + strn(tdetycol), 1024
    fxaddpar, h2, 'TLMAX' + strn(detxcol), 6144
    fxaddpar, h2, 'TLMAX' + strn(detycol), 1024
    fxaddpar, h2, 'TLMAX' + strn(xcol), 6144
    fxaddpar, h2, 'TLMAX' + strn(ycol), 1024
    fxaddpar, h2, 'DET_XSIZ', 7100
    fxaddpar, h2, 'DET_YSIZ', 1025
endelse

print, 'Writing out events...'
fxbcreate, unit, outfil, h2
back = string(replicate(8b,4))
step = float(neve/100)
for i=0l, neve-1 do begin
    if i/step eq fix(i/step) then print, "$(i3,a,$)",100.*i/neve,'%'+back
    fxbwrite, unit, plist(i).time, tcol, i+1
    fxbwrite, unit, -1, expnocol, i+1
    fxbwrite, unit, plist(i).chipx, chipxcol, i+1
    fxbwrite, unit, plist(i).chipy, chipycol, i+1
    fxbwrite, unit, plist(i).detx, detxcol, i+1
    fxbwrite, unit, plist(i).dety, detycol, i+1
    fxbwrite, unit, plist(i).tdetx, tdetxcol, i+1
    fxbwrite, unit, plist(i).tdety, tdetycol, i+1
    fxbwrite, unit, -1, xcol, i+1
    fxbwrite, unit, -1, ycol, i+1
    fxbwrite, unit, plist(i).raw_phas, rawphacol, i+1
    fxbwrite, unit, plist(i).ccdid, ccdidcol, i+1
    fxbwrite, unit, plist(i).pha, phacol, i+1
    fxbwrite, unit, plist(i).grade, gradecol, i+1
    fxbwrite, unit, plist(i).fltgrade, fltgradecol, i+1
    fxbwrite, unit, -1, shellcol, i+1
    if type ne 'ACIS' then fxbwrite, unit, plist(i).order, ordercol, i+1
    fxbwrite, unit, -1., zcoscol, i+1
    fxbwrite, unit, -1., ycoscol, i+1
    fxbwrite, unit, -1., xcoscol, i+1
    fxbwrite, unit, plist(i).zpos, zposcol, i+1
    fxbwrite, unit, plist(i).ypos, yposcol, i+1
    fxbwrite, unit, -1., xposcol, i+1
    fxbwrite, unit, plist(i).energy, ecol, i+1
endfor
fxbfinish, unit

return
end
