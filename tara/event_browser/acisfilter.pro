;+
;==============================================================================
;;; FILE NAME:    $Id: acisfilter.pro 2090 2004-10-26 15:46:41Z patb $
;;;
;;; DESCRIPTION:  A program to filter x-ray events in various ways, and write
;;;               the filtered events out as a spectrum or an event list.
;;;               The intent of this code is to duplicate Event Browser
;;;               filtering.
;;;
;;; AUTHOR:       Scott Koch (tsk@astro.psu.edu)
;;;               Pat Broos  (patb@astro.psu.edu)
;;;               Copyright (C) 1997, Pennsylvania State University
;;;
;;; NOTES:        The parameter 'specfile' is the name of the output spectral
;;;               file.
;;;
;;;               The parameter 'split_threshold' is used to specify the
;;;               split event threshold used in event selection.
;;;               
;;;               The 'dn2evfile' contains a table of gains and offsets for
;;;               a given detector.  Look in /bulk/pkg/xray/idl_lib/pro/*dn2ev
;;;               for a list of detectors contained in the system library.
;;;
;;;               The 'status' parameter is set to "0" on input.  It is set to
;;;               "1" on output if an error occurs.
;;;
;;;               The keyword CCD_ID allows users to select data from multiple
;;;               CCDs. Invalid ACIS CCD_IDs are **silently** ignored.
;;;
;;;               The keyword AMP_ID allows users to select data from multiple
;;;               amplifiers.  Invalid ACIS AMP_IDs are **silently** ignored.
;;;
;;;               The TIME keyword is a 2-element, double-precision vector
;;;               of the for [tstart,tstop].  Events with timestamps outside
;;;               the range [tstart,tstop] are filtered away.  The endpoints,
;;;               tstart and tstop, are NOT filtered away.
;;;
;;;               Supply a list of ACIS grades to retain in ACIS_GRADES.
;;;
;;;               Set the flags G0-G7 to consider only events with the given
;;;               ASCA grade.
;;;
;;;               Set the NO_G255 flag to throw away events which have ACIS
;;;               grade 255.
;;;
;;;               The CIRCLE keyword allows users to specify a circular
;;;               spatial region of events to be included.  "circle" is a
;;;               3-element vector of the form [xcenter, ycenter, radius].
;;;               Border events are NOT filtered away.
;;;
;;;               The RECTANGLE keyword specifies a rectangular spatial region
;;;               of events to be included.  "rectangle" is a 4-element vector
;;;               of the form [xmin,ymin,xmax,ymax].  Border events are NOT
;;;               filtered away.
;;;
;;;               The ANNULUS keyword allows users to specify an annular
;;;               spatial region of events to be included.  "annulus" is a
;;;               4-element vector of the form [xcenter, ycenter, inner_radius,
;;;               outer_radius].  Border events are NOT filtered away.
;;;
;;;               When doing spatial filtering, the number of events included
;;;               in the spatial region is output in the variable 
;;;               "events_in_region".  ONLY a time filter affects this number.
;;;
;;;               When the ONTIME keyword is set, AcisFilter() computes the
;;;               total time the detector was active.  "ontime" is a 3-element
;;;               vector of the form [time, exposure_time, threshold], where
;;;               "time" (or ontime(0)) is the calculated ONTIME returned to
;;;               the client.  "exposure_time" is the integration time for
;;;               each CCD frame, and "threshold" is used to filter away
;;;               bad frames (no events).  The current algorithm is to
;;;               assume that frames which contain no events were thrown
;;;               away by the hardware, so they are not considered to in the
;;;               ontime calculation.
;;;
;;;               Use the TARA routine ReadEvents to fetch x-ray event
;;;               data from files before calling AcisFilter.
;;;		  NOTE that if you want to filter the same data in multiple
;;;		  ways you can load the data once with ReadEvents and then
;;;		  call AcisFilter as many times as you wish.
;;;
;;;               Clients must set either the DN or EV keyword to indicate
;;;               the type of event data passed in and which way the output
;;;               spectral file is binned.  If neither is set, then print an
;;;               error and halt execution.  Do the same if both are set.
;;;
;;;               Use the /PI flag to work in PI space.  Use the /DN flag
;;;               to work in DN space.  Use the /EV flag to work in units of
;;;               eV.  These flags are mutually exclusive.
;==============================================================================
;-
;==============================================================================
PRO AcisFilter, specfile, split_threshold, dn2evfile, status, events_in_region,$
		CCD_ID=ccd, AMP=amp, TIME=time, $
		ACIS_GRADES=acis_grades, $
		G0=g0, G1=g1, G2=g2, G3=g3, G4=g4, G5=g5, G6=g6, G7=g7, $
		NO_G255=no_g255, CIRCLE=circle, RECTANGLE=rectangle, $
		ONTIME=ontime, USE_QUAL=use_qual, DN=dn_flag, EV=ev_flag, $
		PI=pi_flag, ANNULUS=annulus

;==============================================================================
; Update the parameter data structures.
dum = GetParameter('split_event_threshold', split_event_threshold)
*(*split_event_threshold).data = split_threshold

dum = GetParameter('dn_ev_file', dn_ev_file)
*(*dn_ev_file).data = dn2evfile

;==============================================================================
; /EV, /DN, or /PI must be set.
if (     keyword_set(dn_flag) AND $
     NOT keyword_set(ev_flag) AND $
     NOT keyword_set(pi_flag)) then begin
  ev_flag = 0
  pi_flag = 0
endif else if (    keyword_set(ev_flag) AND $
               NOT keyword_set(dn_flag) AND $
               NOT keyword_set(pi_flag)) then begin
  dn_flag = 0
  pi_flag = 0
endif else if (    keyword_set(pi_flag) AND $
               NOT keyword_set(ev_flag) AND $
               NOT keyword_set(dn_flag)) then begin
  ev_flag = 0
  dn_flag = 0
endif else begin
  message, 'You must set one of the following flags:  /DN, /EV, or /PI.'
endelse

; Initialize
status = 0
nevents = CountXrayData()
mask = make_array(nevents, /BYTE, VALUE=1)
this_mask = bytarr(nevents)
;==============================================================================
;
; Filter by CCD_ID
;
;==============================================================================
if( n_elements(ccd) GT 0 ) then begin
  print, '  Filtering by CCD_ID...'
  if (0 EQ GetProperty( 'ccd_id', DDS_DATA=ccd_id )) then $
    message, 'ccd_id not available'

  ccd = [ccd]
  ccd_mask = bytarr(nevents)

  for ii=0,n_elements(ccd)-1 do begin
    if( (ccd[ii] GE 0) AND (ccd[ii] LE 9) ) then begin
      indices  = where (*ccd_id EQ ccd[ii], count)
      if( count GT 0) then begin
        ccd_mask(indices) = 1
      endif
    endif
  endfor
  
  index = where(ccd_mask EQ 1, count)
  if (count EQ 0) then begin
    print, '    Dataset empty after filter operation.'
    status = 1
  endif
  mask = temporary(mask) AND ccd_mask
endif
;==============================================================================
;
; Filter by AMP
;
;==============================================================================
if( n_elements(amp) GT 0 ) then begin
  print, '  Filtering by amplifier...'
  if (0 EQ GetProperty( 'amp_id', DDS_DATA=amp_id)) then $
    message, 'amp_id not available'

  amp = [amp]
  this_mask = bytarr(nevents)

  for ii=0,n_elements(amp)-1 do begin
    if( (amp[ii] GE 0) AND (amp[ii] LE 3) ) then begin
      indices  = where (*amp_id EQ amp[ii], count)
      if( count GT 0) then begin
        this_mask(indices) = 1
      endif
    endif
  endfor
  
  index = where(this_mask EQ 1, count)
  if (count EQ 0) then begin
    print, '    Dataset empty after filter operation.'
    status = 1
  endif
  mask = temporary(mask) AND this_mask
endif
;==============================================================================
;
; Get rid if events which are flagged as "bad" in the quality code vector
;
;==============================================================================
if( keyword_set(use_qual) ) then begin
  print, '  Filtering poor quality events...'
  if (0 EQ GetProperty( 'qualcode', DDS_DATA=qualcodes )) then $
    message, 'qualcode not available'

  this_mask(*) = 0
  indices = where(*qualcodes EQ 0, count)
  if( count GT 0) then begin
      this_mask(indices) = 1
  endif else begin
    print, '    Dataset empty after filter operation.'
    status = 1
    return
  endelse
  mask = temporary(mask) AND this_mask
endif
;==============================================================================
;
; Filter by TIME.  Here we'll create a mask of valid times.  Store this mask
;                  in a variable which will not be overwritten, because it
;                  will be used later for calculating ONTIME.
;
;==============================================================================
if (0 EQ GetProperty( 'TIME', DDS_DATA=times )) then $
  message, 'TIME not available'

if( keyword_set(TIME) ) then begin
  print, '  Filtering by time...'
  time_mask = bytarr(nevents)
  indices = where ((*times GE time(0)) AND (*times LE time(1)), count)
  if( count GT 0) then begin
    time_mask(indices) = 1
  endif else begin
    print, '    Dataset empty after filter operation.'
    status = 1
    return
  endelse
  mask = temporary(mask) AND time_mask
endif else begin
  time_mask = make_array(nevents, /DOUBLE, VALUE=1.0)
endelse
;==============================================================================
;
; Filter spatially
;
;==============================================================================
if(keyword_set(CIRCLE) OR keyword_set(RECTANGLE) OR keyword_set(ANNULUS)) $
then begin
  if (0 EQ GetProperty( 'x_position', DDS_DATA=x )) then $
    message, 'x_position not available'

  if (0 EQ GetProperty( 'y_position', DDS_DATA=y )) then $
    message, 'y_position not available'
endif

if( keyword_set(ANNULUS) ) then begin
  print, '  Filtering an annular region...'
  x_center     = float(annulus(0))
  y_center     = float(annulus(1))
  inner_radius = float(annulus(2))
  outer_radius = float(annulus(3))
  this_mask(*) = 0
  dist = (*x-x_center)^2 + (*y-y_center)^2
  indices=where((outer_radius^2 GE dist) AND (inner_radius^2 LE dist), count)
  if( count GT 0) then begin
      this_mask(indices) = 1
      dummy = where((this_mask EQ 1) AND (time_mask EQ 1.0), events_in_region)
  endif else begin
    print, '    Dataset empty after filter operation.'
    status = 1
    return
  endelse
  mask = temporary(mask) AND this_mask
endif

if( keyword_set(CIRCLE)) then begin
  print, '  Filtering a circular region...'
  x_center = float(circle(0))
  y_center = float(circle(1))
  radius   = float(circle(2))
  this_mask(*) = 0
  indices = where(radius^2 GE ((*x-x_center)^2 + (*y-y_center)^2), count)
  if( count GT 0) then begin
      this_mask(indices) = 1
      dummy = where((this_mask EQ 1) AND (time_mask EQ 1.0), events_in_region)
  endif else begin
    print, '    Dataset empty after filter operation.'
    status = 1
    return
  endelse
  mask = temporary(mask) AND this_mask
endif

if( keyword_set(RECTANGLE)) then begin
  print, '  Filtering a rectangular region...'
  this_mask(*) = 0
  indices = where ( *x GE rectangle(0) AND  *x LE rectangle(2) AND $
		    *y GE rectangle(1) AND  *y LE rectangle(3), count )
  if(count GT 0) then begin
      this_mask(indices) = 1
      dummy = where((this_mask EQ 1) AND (time_mask EQ 1.0), events_in_region)
  endif else begin
    print, '    Dataset empty after filter operation.'
    status = 1
    return
  endelse
  mask = temporary(mask) AND this_mask
endif
;==============================================================================
;
; Filter By Grade
;
;==============================================================================
grademask = bytarr(8)
if( keyword_set( g0 )) then begin
  grademask(0) = 1
endif
if( keyword_set( g1 )) then begin
  grademask(1) = 1
endif
if( keyword_set( g2 )) then begin
  grademask(2) = 1
endif
if( keyword_set( g3 )) then begin
  grademask(3) = 1
endif
if( keyword_set( g4 )) then begin
  grademask(4) = 1
endif
if( keyword_set( g5 )) then begin
  grademask(5) = 1
endif
if( keyword_set( g6 )) then begin
  grademask(6) = 1
endif
if( keyword_set( g7 )) then begin
  grademask(7) = 1
endif

if (total(grademask) GT 0) then begin
  if (0 EQ GetProperty( 'asca_grade', DDS_DATA=asca_grades )) then $
    message, 'asca_grade not available'

  this_mask(*) = 0
  for grade = 0, 7 do begin
    if( grademask(grade) EQ 1 ) then begin
      index = where(*asca_grades EQ grade, count )
      if (count GT 0) then this_mask[index] = 1
    endif
  endfor
  mask = temporary(mask) AND this_mask
endif


if (n_elements(acis_grades) GT 0) then begin
  if (0 EQ GetProperty( 'instrument_grade', DDS_DATA=instrument_grades )) then $
    message, 'instrument_grade not available'

  this_mask(*) = 0
  for ii = 0, n_elements(acis_grades)-1 do begin
      index = where(*instrument_grades EQ acis_grades[ii], count )
      if (count GT 0) then this_mask[index] = 1
  endfor
  mask = temporary(mask) AND this_mask
endif


if(keyword_set(no_g255)) then begin
  print, 'Filtering grade 255 events'
  if (0 EQ GetProperty( 'instrument_grade', DDS_DATA=instrument_grades )) then $
    message, 'instrument_grade not available'

  indices = where(*instrument_grades EQ 255, count)
  if( count GT 0 ) then mask(indices) = 0
endif

indices = where( mask EQ 1, count)
if(count EQ 0 ) then begin
  print, '  Dataset empty after filter operation.'
  status = 1
endif
;==============================================================================
;
; Calculate the CCD ONTIME.
;
;==============================================================================
if( n_elements(ontime) EQ 3 ) then begin
  ; We're looking for telemetry dropouts here.  Such dropouts occur when
  ; the telemetry is saturated.  The ACIS BEP determines that there isn't
  ; room to store events from a given CCD, so the entire CCD frame is
  ; thrown away.  To look for these, filter by time and by CCD, make a
  ; light curve, then apply a threshold filter.  Do NOT filter by amplifier
  ; ID, since this is not physical.  Individual amps do not drop out (unless
  ; they're broken), so ONTIME is defined as the time when the chip is on.
  print, '  Calculating ONTIME...'
  indices = where((time_mask EQ 1) AND (ccd_mask EQ 1), $
		   count)
  if( count GT 0 ) then begin
    filtered_time = (*times)(indices)
    light_curve = histogram(filtered_time, MIN=min(filtered_time), $
			    BINSIZE=ontime(1))
    indices = where(light_curve GE ontime(2),count)
    if( count GE 0 ) then begin
      ontime(0) = n_elements(indices) * ontime(1)
    endif else begin
      print, "    No bins above specified threshold of ", ontime(2)
      print, "    ONTIME is zero."
      ontime(0) = 0.0
    endelse
  endif else begin
    print, '    Dataset empty after time, ccd, and amp filter operation.'
    print, '    ONTIME is zero.'
    ontime(0) = 0.0
  endelse
endif else if (n_elements(ontime) EQ 0) then begin
  ; do nothing
endif else begin
  print, '    Ontime is specified incorrectly.'
  print, '        ontime=[ontime,frametime,counts_threshold]'
  print, '    No output produced.'
  status = 1
  return
endelse
;==============================================================================
;
; Now that all bad events have been marked in the filter mask array, filter
; the data array(s).
;
;==============================================================================
SetWdsMask, mask

if (CountXrayData(/WORKING_DATASET) EQ 0) then begin
  print, 'Dataset empty'
  status = 1
  return
endif

;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;==============================================================================
;
; Filtration is done.  Below this point, you may do calculations on the
;                      filtered data.
;
;==============================================================================
; Write an event file

;==============================================================================
; Write a spectral file
if(ev_flag) then begin
  prop_name = 'energy'
endif else if (dn_flag) then begin
  prop_name = 'pha'
endif else if (pi_flag) then begin
  prop_name = 'pi'
endif else begin
  message, "Energy units in event file not specified.  Use /DN, /EV, or /PI."
endelse

if (0 EQ GetProperty( prop_name, WDS_DATA=amplitudes )) then $
  message, prop_name + ' not available'

if(ev_flag) then begin
  WriteSpectralFile, amplitudes, specfile, /EV
endif else if (dn_flag) then begin
  WriteSpectralFile, amplitudes, specfile, /DN
endif else if (pi_flag) then begin
  WriteSpectralFile, amplitudes, specfile, /PI
endif else begin
  message, "Energy units in event file not specified.  Use /DN, /EV, or /PI."
endelse

RETURN
END
