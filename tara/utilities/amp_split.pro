;;; $Id: amp_split.pro 2320 2005-11-11 22:07:02Z patb $
;;; A tool for splitting calibration data by amplifier, CTI correcting, and merging.
;;; Patrick Broos
;;;
;;; Usage:  amp_split, 'y2000', 'y2000_catalog.txt', /SPLIT
;;;         amp_split, 'y2000', /MERGE
;;;
;;; Only CCDs I0-I3, S2, S3 are processed because the CTI corrector does not support the others.

; Load acis_extract to get run_command program.
@acis_extract

PRO amp_split, destination_directory, catalog_file, SPLIT=split, MERGE=merge

run_command, /INIT, PARAM_DIR='cxcds_param'
;; Get rid of pre-existing configuration for the CIAO commands we'll use below.
run_command, /QUIET, ['punlearn dmcopy', 'pset dmcopy clobber=yes','punlearn acis_process_events']
  
print, 'A temperature of -120C is assumed!'

cd, CURRENT=basedir

ccd_list = [0,1,2,3,6,7]
amp_list = [0,1,2,3]
make_2d, amp_list, ccd_list
amp_dirs = destination_directory + '/c' + string(ccd_list,F='(I0)') + '_n' + string(amp_list,F='(I0)') + '/'

if keyword_set(split) then begin
  file_mkdir, destination_directory
  file_mkdir, amp_dirs
  
  ape_filename = destination_directory + '/ape.evt'
  
  readcol, catalog_file, obs_filename, FORMAT='A', COMMENT=';'
  obs_filename = strtrim(obs_filename,2)
  num_obs = n_elements(obs_filename)
  
  for ii=0,num_obs-1 do begin
    print & print, '================================='
    print, 'Processing ', obs_filename[ii]
    
    ; Run acis_process_events to get latest calibration and to remove CTI correction.
    cmd = string(obs_filename[ii], ape_filename, $
                   F="(%'acis_process_events infile=%s outfile=%s acaofffile=NONE apply_cti=no apply_tgain=no clobber=yes')")
    run_command, cmd

    for jj=0,n_elements(amp_dirs)-1 do begin
      ;; Status filter, split by amp, remove useless columns.
      dest_filename = amp_dirs[jj]+obs_filename[ii]
      cmd = string(ape_filename, ccd_list[jj], amp_list[jj], dest_filename, $
                   F="(%'dmcopy ""%s[status=0,ccd_id=%d,node_id=%d][cols -expno,-tdet,-det,-sky]"" %s')")
      run_command, cmd
      
      num_events = sxpar(headfits(dest_filename, EXT=1), 'NAXIS2')
      case ccd_list[jj] of
        4: do_correction=0
        5: do_correction=0
        8: do_correction=0
        9: do_correction=0
        else: do_correction=1
      endcase
      
      if (num_events EQ 0) then begin
        file_delete, dest_filename
      endif else if (do_correction) then begin
        ;; Run the CTI corrector.
        cd, amp_dirs[jj]
        
        ; Try to avoid the interactive 'rm' commands in correctit.
        ; It's not easy to avoid the interactive 'mv' commands at the end (if the same file 
        ; is processed twice) -- we would have to replicate here the file name computaton
        ; that's done in correct_cti.pro.
        file_delete, "correct_cti.log", "correctit.temp1", "correctit.temp2", /ALLOW_NONEXISTENT
        
        cmd = 'correctit ' + obs_filename[ii]
        run_command, cmd
        cd, basedir
      endif
    endfor ;jj
    
    file_delete, ape_filename
  endfor ;ii
  
  ;; Clean up trash.
  print, 'Cleaning up trash ...'
  trash = file_search(amp_dirs+"*recalc*", COUNT=count)
  if (count GT 0) then file_delete, trash

  trash = file_search(amp_dirs+"correct_cti.log", COUNT=count)
  if (count GT 0) then file_delete, trash

  file_delete, file_search('cxcds_param','*'), 'cxcds_param'
endif



if keyword_set(merge) then begin
  ;; Look in each amp dir and merge all the data you find.
  for jj=0,n_elements(amp_dirs)-1 do begin
    if (NOT file_test(amp_dirs[jj])) then continue
    
    merged_events_fn = amp_dirs[jj]+ ['merged.evt1.fits','merged_no_cti.evt1.fits']
    file_delete, merged_events_fn, /ALLOW_NONEXISTENT

    filenames = file_search( amp_dirs[jj],"*", /TEST_REGULAR, COUNT=count)
    if (count EQ 0) then continue
    
    no_cti_ind = where(strmatch(filenames, '*no_cti*'), no_cti_count, COMPLEMENT=ind, NCOMP=count)
    if (count GT 0) then begin
      cmd = string( strjoin(filenames[ind]," "), merged_events_fn[0],$
                F="(%'dmmerge ""%s"" columnList="""" outfile=%s outBlock="""" lookupTab=""""')")
      run_command, cmd
    endif
    
    if (no_cti_count GT 0) then begin
      cmd = string( strjoin(filenames[no_cti_ind]," "), merged_events_fn[1],$
                F="(%'dmmerge ""%s"" columnList="""" outfile=%s outBlock="""" lookupTab=""""')")
      run_command, cmd
    endif
  endfor
  
endif

return
end
