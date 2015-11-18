;+
;========================================================================
;;;
;;; FILE NAME:    $Id: eb.pro 2954 2008-01-16 22:07:50Z patb $
;;;
;;; DESCRIPTION:  Event Browser Application
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        See the document "X-ray Event Browser Software Design
;;;               Specification"
;;;

;;; Keywords passed on to CreateManagerWidget are:
;;; GENERIC: set to use for non-xray data
;;; KEYWORDS2COLUMNS: list of header keywords that should be used to
;;;                   define event properties
;;;
;-
;========================================================================

PRO eb, RECOVER=recover_flag, BLOCK=block, XRT=xrt, _EXTRA=extra

FORWARD_FUNCTION GetParameter

;; Get into DirectColor if possible.
color_manager


;; Compile some of the EB code to avoid delays later.
;resolve_routine, 'dataset_1d'
;resolve_routine, 'dataset_2d'

resolve_routine, 'eb_parameter'
resolve_routine, 'eb_property'
resolve_routine, 'eb_derived_property'
resolve_routine, 'domain_dataset'

;resolve_routine, 'eb_dataset_1d', /IS_FUNCT
;resolve_routine, 'eb_dataset_2d', /IS_FUNCT

resolve_routine, 'eb_manager'
resolve_routine, 'eb_custom_filter'

if (NOT keyword_set(recover_flag)) then recover_flag = 0

InitializeDomainDataset, recover_flag, /GUI_ACTIVE, /UNDEFINE_PROPERTIES
;widget_control, /RESET

if keyword_set(xrt) then begin
  resolve_routine, 'eb_custom_filter_xrt'
  print, (routine_info('eb_custom_filter_xrt',/SOURCE)).path, F='(%"\nLoaded custom filter %s")'

  dum = GetParameter('ev_per_dn', ev_per_dn)
  value = 2.56
  *(*ev_per_dn).data = value
  print, value, F='(%"Set ev_per_dn paramter to %4.2f")'

  dum = GetParameter('ev_dn_offset', ev_dn_offset)
  *(*ev_dn_offset).data = 0

  dum = GetParameter('split_event_threshold', split_event_threshold)
  value = 40
  *(*split_event_threshold).data = value
  print, value, F='(%"Set ev_per_dn paramter to %2d")'
endif

CreateManagerWidget, recover_flag, keyword_set(block), _EXTRA=extra
xmanager

; Add an entry to the log file.
logfile = '~patb/TARA/code/event_browser/public_usage.log'
if file_test(logfile, /WRITE) then begin
  openw, unit, logfile, /GET_LUN, /APPEND, ERROR=error
  if (error NE 0) then begin
    print, 'ERROR opening log file:' + logfile
  endif else begin
    get_date, date
    printf, unit, date + " " + getenv('USER') + " " + !VERSION.OS
    free_lun, unit
  endelse
endif

return
end

