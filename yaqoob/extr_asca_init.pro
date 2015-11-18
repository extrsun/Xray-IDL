pro extr_asca_init, obsname, det, unflist, hpcut, gradeflt, xybin=xybin, $
                    recover_time, date=date, outevtfil=outevtfil, loglun=loglun

if n_params(0) eq 0 then begin
    print,'extr_asca_init, obsname, det, unflist, hpcut, gradeflt, xybin=xybin, $'
    print,'                recover_time, date=date, outevtfil=outevtfil, loglun=loglun'
    print,'Creates initial xselect processing script, called from extr_asca'
    retall
endif

if n_elements(date) eq 0 then date = ''
if n_elements(xybin) eq 0 then xybin = 1
if n_elements(loglun) eq 0 then loglun = -1 

det_name = ['sis0', 'sis1', 'gis2', 'gis3']
short_det_name = ['s0', 's1', 'g2', 'g3']
bitrate_name = ['low', 'medium', 'high']

; mkf params that apply to both sis and gis
mkf = 'select mkf "saa.eq.0 .and. elv.gt.5 .and. fov.eq.0 .and. cor.gt.6 ' + $
  '.and. acs.eq.0 .and. ang_dist.gt.0 .and. ang_dist.lt.0.01'

; Open script file
sname = obsname + "_" + short_det_name(det) + "_init_" + date + ".xco"
flist = findfile(sname, count=count)
if count gt 0 then begin
  printf, loglun, sname + ' already exists, delete it if you want the unf'
  printf, loglun, ' files to be reprocessed'
  return
endif

flist = findfile('../aux/*mkf*', count=count)
if count eq 0 then begin
  print, 'No mkf files found in ../aux, please install them to do minimal mkf cleaning'
  print, 'Returning from extr_asca_init.pro'
  return
endif
extr_asca_gunzip, flist

openw, lun, sname, /get_lun

; Prelimenary mkf selection can be applied to both sis and gis
; gis can have the high-bgd ring and calib src. removed
; sis can have hot pixels removed
; first persistant hot pixels are removed, then grades 1 and >4 are
; removed and flickering hot pixels are reduced

printf, lun, "xsel"
printf, lun, "set instr " + det_name(det)

; First read the individual unf files back in
for i=0, n_elements(unflist) - 1 do $
  printf, lun, "read eve " + unflist(i) + " ."

root = obsname + "_" + short_det_name(det) + "_" + date

if det lt 2 then begin ; SIS
  printf, lun, $
    "sisclean saoimage=no sis_plot=no clean=1 bkg_thr=" + strn(hpcut) + $
    " clean_phalow=0 clean_phahi=4095"
  printf, lun, "extr eve
  hpcut_name = root + "_hc" + strn(hpcut) + ".evt"
  printf, lun, "save eve " + hpcut_name + " use_events=yes clobber=yes"
  printf, lun, "clear all"
  printf, lun, "yes"
  printf, lun, "set instr " + det_name(det)
  printf, lun, "read eve " + hpcut_name + " ."
  printf, lun, 'select eve "' + gradeflt + '"'
  printf, lun, mkf + '.and. ((t_dy_nt.lt.0) .or. (t_dy_nt.gt.' + $
    strn(recover_time) + ')) .and. ((t_saa.lt.0) .or. (t_saa.gt.' + $
    strn(recover_time) + ')) .and. br_earth.gt.10" mkf_dir=../aux/'
  printf, lun, "sisclean saoimage=no sis_plot=no clean=2 cellsize=5 " + $
    "log_prob=-5.24  bkg_thr=2 clean_phalow=0 clean_phahi=4095"
endif else begin
  openw, lun2, det_name(det) + "_nocalib.reg", /get_lun
  if det eq 2 then begin
    printf, lun2, " CIRCLE(124,132,81.00)"
    printf, lun2, "-CIRCLE(166.00,221.0,24.00)"
  endif else begin
    printf, lun2, " CIRCLE(133.0,119.0,73.00)"
    printf, lun2, "-CIRCLE(205.0,99.0,8.0)"
  endelse
  free_lun, lun2
  printf, lun, "filter region " + det_name(det) + "_nocalib.reg"
  printf, lun, mkf + ".and." + short_det_name(det) + $
    '_l1.gt.0" mkf_dir=../aux/'
endelse

printf, lun, "extr eve"
printf, lun, "save eve " + root + "_mkf.evt use_events=yes clobber=yes"
printf, lun, "set xybin " + strn(xybin)
printf, lun, "extr im"
printf, lun, "save im " + root + "_mkf.img clobber=yes"
printf, lun, "exit"
printf, lun, "no"

free_lun, lun
spawn,'xselect @'+sname
outevtfile = root + "_mkf.evt" 

return
end
