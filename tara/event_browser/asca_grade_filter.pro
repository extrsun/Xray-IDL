;+
;========================================================================
;;;
;;; FILE NAME:    $Id: asca_grade_filter.pro 610 1998-08-11 13:17:59Z patb $
;;;
;;; DESCRIPTION:  ASCA Grade Filter Widget for X-ray Event Browser application.
;;;               The manager_widget parameter is the widget ID of the 
;;;               Manager Widget (the widget that is calling this function).
;;;
;;;               The UVALUE of the first child of the base is used to store
;;;               a structure representing the state of the filter widget.
;;;               In this state is an IDL handle that is used to store the
;;;               byte mask array for the filter, which form the VALUE
;;;               property of the widget itself.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        See the document "X-ray Event Browser Software Design
;;;               Specification"
;;;
;-
;========================================================================

;;; The relationship between ASCA and ACIS grades is fully described in 
;;; the document ACIS Grading Scheme Recommendation by Kenny Glotfelty
;;; at the ASC -- see http://hea-www.harvard.edu/acis/xrcf/grade.html.

;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO AscaGradeFilterCleanup, top_base

widget_control, top_base, GET_UVALUE=state, /NO_COPY

ptr_free, state.filter_mask_ptr

;; Send an event to the manager.
  new_event = $
	 { FILTER_REVISED, ID:state.manager_widget, TOP:state.manager_widget, $
	   HANDLER:0L, NAME:'filter_revised', FILTER_ID:state.top_base, $
	   DESCRIPTION: 'defunct filter' }

  widget_control, state.manager_widget, SEND_EVENT=new_event, BAD_ID=bad_id

return
end


;==========================================================================
PRO AscaGradeFilterEvent, event

filter_revised = 0
destroy_flag   = 0

;; Get the widget state structure.
top_base = event.handler
widget_control, top_base, GET_UVALUE=state, /NO_COPY, $
		/HOURGLASS


;; Handle the event; branch first based on which widget generated it.
case event.id of

  ;------------------------------------------------------------------------
  state.top_base: $
    begin
    case event.name of
      'working_set_stale': 

      'working_set_delete': 

      'working_set_fresh': 

      'domain_set_revised' : state.compute_mask_flag = 1

      else: message, 'event not handled'
    endcase
    end

  ;------------------------------------------------------------------------
  state.buttons: 	 filter_revised = 1

  ;------------------------------------------------------------------------
  state.acis255_control: $
    begin
    state.acis255_mode = event.index 
    filter_revised = 1
    end

  ;------------------------------------------------------------------------
  state.destroy_button: $
    begin
    filter_revised = 1
    destroy_flag   = 1
    end

  ;------------------------------------------------------------------------
  else: message, 'event not handled'
endcase


;; If the filter mask was revised, create an event for the Manager Widget.
if (filter_revised) then begin

  new_event = $
    { FILTER_REVISED, ID:state.manager_widget, TOP:state.manager_widget, $
      HANDLER:0L, NAME:'filter_revised', FILTER_ID:state.top_base, $
      DESCRIPTION: 'grade filter' }

  widget_control, state.manager_widget, SEND_EVENT=new_event

  state.compute_mask_flag = 1
endif 

 
;; Save the state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Destroy the widget if desired.
if (destroy_flag) then widget_control, top_base, /DESTROY

return
end


;==========================================================================
FUNCTION GetAscaGradeFilterValue, top_base

;; Get the widget state structure.
widget_control, top_base, GET_UVALUE=state, /NO_COPY

;; If necessary, recompute the filter mask.
if (state.compute_mask_flag) then begin
  state.compute_mask_flag = 0
  
  z=GetProperty('instrument_grade', DDS_DATA=ACIS_grades)
  z=GetProperty('asca_grade',       DDS_DATA=asca_grades)

  ; Get the button flags.
  widget_control, state.buttons, GET_VALUE=button_flags
  button_flags = byte( button_flags )

  ; Make the mask vector.
  mask = replicate( button_flags(7), CountXrayData() > 1 )
  
  for grade = 0, 6 do begin
    index = where( *asca_grades EQ grade, count )
    if (count GT 0) then mask[index] = button_flags[grade]
  endfor
  
  index = where( *ACIS_grades EQ 255, count )
  if ((count GT 0) AND (state.acis255_mode EQ 1)) then $
    mask[index] = 1
    
  if ((count GT 0) AND (state.acis255_mode EQ 2)) then $
    mask[index] = 0

  ; Store the completed mask array.
  ptr_free, state.filter_mask_ptr
  state.filter_mask_ptr = ptr_new(mask, /NO_COPY)
endif

filter_mask = *state.filter_mask_ptr

;; Save the state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

return, filter_mask
end


;========================================================================
FUNCTION asca_grade_filter, manager_widget, $
	 UVALUE = uvalue, TITLE=title, GROUP=group 

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'ASCA Grade Filter'
if NOT keyword_set(group)  then group  = 0

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

top_base = widget_base( UVALUE=uvalue, TITLE=title, GROUP=group, /COLUMN, $
	                FUNC_GET_VALUE='GetAscaGradeFilterValue', $
			/BASE_ALIGN_CENTER, XOFFSET=0, YOFFSET=270 )

 names = ['g0 (ACIS 0)', $
	  'g1 (ACIS 1,4,5,32,33,36,37,128,129,132,133,160,161,164,165)', $
	  'g2 (ACIS 2,34,64,65,68,69,130,162)', $
	  'g3 (ACIS 8,12,136,140 OR 16,17,48,49)',$
	  'g4 (ACIS 16,17,48,49  OR 8,12,136,140)', $
	  'g5 (ACIS 3,6,9,13,20,21,35,38,40,44,52,53,96,97,100,101,131,134,137,141,144,145,163,166,168,172,176,177,192,193,196,197)', $
	  'g6 (ACIS 10,11,18,22,50,54,72,76,80,81,104,108,138,139,208,209)', $
	  'g7 (others)']

 buttons = cw_bgroup( top_base, names, /COL, /NONEXCLUSIVE, $
			/SPACE, /XPAD, /YPAD, LABEL_TOP='ASCA Grades', $
			/SCROLL, X_SCROLL_SIZE=180, Y_SCROLL_SIZE=230 )

 widget_control, buttons, SET_VALUE=[1,0,1,1,1,0,1,0]

 tit = 'ACIS 255:'
 styles = ['Follow g7', 'Include', 'Exclude']
 acis255_control = widget_droplist( top_base, TITLE=tit, VALUE=styles )

 destroy_button = widget_button( top_base, VALUE='Dismiss Filter',/ALIGN_RIGHT)


;; Store the widget state.

state = { $
	; The id of the Manager Widget.
	manager_widget:manager_widget, $

	; Handle for local mask.
	filter_mask_ptr: ptr_new(/ALLOC), $

	;IDs of widgets that generate events or have to be updated.
	;Many of these widgets have "value" slots that hold state information.
	top_base:top_base, buttons:buttons, acis255_control:acis255_control,$
	destroy_button:destroy_button, $

	; Other state information.
	acis255_mode:0B, compute_mask_flag:1 }

widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Realize the widget and register with manager.
widget_control, top_base, /REALIZE

;; Simulate revision of the filter to generate an event for the manager widget.
AscaGradeFilterEvent, {ID:buttons, TOP:buttons, HANDLER:top_base}

xmanager, 'ASCA grade filter', top_base, GROUP_LEADER=group, $
	  EVENT_HANDLER='AscaGradeFilterEvent', /NO_BLOCK, $
	  CLEANUP='AscaGradeFilterCleanup'
	  
return, top_base
end


