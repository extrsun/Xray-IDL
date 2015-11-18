;+
;==========================================================================
;;;
;;; FILE NAME:    $Id: acis_grade_filter.pro 4380 2012-10-30 20:20:43Z psb6 $
;;;
;;; DESCRIPTION:  ACIS Grade Filter Widget for X-ray Event Browser application.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        See the document "X-ray Event Browser Software Design
;;;               Specification"
;;;
;;;               The manager_widget parameter is the widget ID of the 
;;;               Manager Widget (the widget that is calling this function).
;;;
;;;               The UVALUE of the first child of the base is used to store a
;;;               structure representing the state of the filter widget. 
;;;               In this state is an IDL handle that is used to store the
;;;               byte mask array for the filter, which form the VALUE
;;;               property of the widget itself.
;;;
;-
;==========================================================================
;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO AcisGradeFilterCleanup, top_base

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
PRO AcisGradeFilterEvent, event

filter_revised = 0
destroy_flag   = 0
redisplay_flag = 0

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

      'domain_set_revised' : $
        begin
        redisplay_flag           = 1
	state.compute_mask_flag  = 1
	end

      else: message, 'event not handled'
    endcase
    end

  ;------------------------------------------------------------------------
  ; These events used only for widget initialization.
  state.button_base: $
    begin
    redisplay_flag = 1
    filter_revised = 1
    end

  ;------------------------------------------------------------------------
  state.entry_list_widget: $
    begin
    state.selected_entry = event.index
    case (state.entry_list( event.index ).status) of
     -1: 
      0: state.entry_list(event.index).status = 1
      1: state.entry_list(event.index).status = 0
    endcase
    filter_revised = 1
    end

  ;------------------------------------------------------------------------
  state.accept_button: $
    begin
    state.entry_list.status = 1
    filter_revised = 1
    end
    
  ;------------------------------------------------------------------------
  state.reject_button: $
    begin
    state.entry_list.status = 0
    filter_revised = 1
    end

  ;------------------------------------------------------------------------
  state.default1_button: $
    begin
    ind = [0, 2, 8, 16, 64]
    state.entry_list[ind].status = ~(state.entry_list[ind].status)
    filter_revised = 1
    end

  ;------------------------------------------------------------------------
  state.default2_button: $
    begin
    ind = [0,2,8,16,64,80,18,10,72,208,22,11,104]
    state.entry_list[ind].status = ~(state.entry_list[ind].status)
    filter_revised = 1
    end

  ;------------------------------------------------------------------------
  state.default3_button: $
    begin
    ind = [24,66,107,214,255]
    state.entry_list[ind].status = ~(state.entry_list[ind].status)
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
      DESCRIPTION: 'ACIS grade filter' }

  widget_control, state.manager_widget, SEND_EVENT=new_event

  redisplay_flag          = 1
  state.compute_mask_flag = 1
endif 


;; If necessary, redraw the entry list widget.
if (redisplay_flag) then begin
  entry_list  = state.entry_list
  num_entries = n_elements(entry_list)

  text = strarr( num_entries )

  for ii=0, num_entries-1 do begin
    entry = entry_list(ii)

    label = string( entry.grade, f='("grade ",I3.3," :")' )

    case (entry.status) of  
     -1: text(ii) = ''
      0: text(ii) = label + 'xxx'
      1: text(ii) = label + 'ACCEPTED'
    endcase
  endfor

  widget_control, state.entry_list_widget, $
		  SET_LIST_SELECT=state.selected_entry, SET_VALUE=text
endif

;; Save the state structure.
;print, state.entry_list.status
widget_control, top_base, SET_UVALUE=state, /NO_COPY



;; Destroy the widget if desired.
if (destroy_flag) then widget_control, top_base, /DESTROY

return
end


;==========================================================================
FUNCTION GetAcisGradeFilterValue, top_base

;; Get the widget state structure.
widget_control, top_base, GET_UVALUE=state, /NO_COPY

;; If necessary, recompute the filter mask.
if (state.compute_mask_flag) then begin
  state.compute_mask_flag = 0

  ; Get the new x-ray data.
  z=GetProperty('instrument_grade', DDS_DATA=grades)

  ; Compute the mask vector.
  mask = replicate( 0B, CountXrayData() > 1 )

  entry_list = state.entry_list
  for ii=0, n_elements(entry_list)-1 do begin
    entry = entry_list(ii)

    if (entry.status EQ 1) then begin
      index = where( *grades EQ entry.grade, event_count )

      if (event_count GT 0) then mask(index) = 1
    endif
  endfor

  ; Store the completed mask array.
  ptr_free, state.filter_mask_ptr
  state.filter_mask_ptr = ptr_new(mask, /NO_COPY)
endif


filter_mask = *state.filter_mask_ptr

;; Save the state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

return, filter_mask
end



;==========================================================================
FUNCTION acis_grade_filter, manager_widget, $
	 UVALUE = uvalue, TITLE=title, GROUP=group 

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'ACIS Grade Filter'
if NOT keyword_set(group)  then group  = 0


;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

top_base = widget_base( UVALUE=uvalue, TITLE=title, GROUP=group, /COLUMN, $
	                FUNC_GET_VALUE='GetAcisGradeFilterValue', $
			/BASE_ALIGN_CENTER, XOFFSET=0, YOFFSET=370 )

 entry_list_widget = widget_list( top_base, XSIZE=20, YSIZE=18 )

 button_base = widget_base( top_base, /ROW, /ALIGN_RIGHT )

 accept_button = widget_button( top_base, VALUE='ACCEPT ALL' )

 reject_button = widget_button( top_base, VALUE='REJECT ALL' )

 default1_button = widget_button( top_base, VALUE='toggle Singles (ACIS 0,2,8,16,64)' )

 default2_button = widget_button( top_base, VALUE='toggle Singles, ells (ACIS 0,2,8,16,64,80,18,10,72,208,22,11,104)' )

 default3_button = widget_button( top_base, VALUE='toggle Bev Grades (ACIS 24,66,107,214,255)' )

 destroy_button = widget_button( top_base, VALUE='Dismiss Filter' )


;; Initialize data structures associated with entry list widget.
;; Status values have the following meanings:
;;  -1: there is no entry at this location in the entry_list array
;;   0: the entry is REJECTED by the user
;;   1: the entry is ACCEPTED by the user

entry={grade: 0, status:0, domain_event_count:0L }
entry_list = replicate( entry, 256 )
entry_list.grade = indgen(256)
;entry_list[[0, 2, 8, 16, 64]].status = 1

;; Store the widget state.

state = { $
	; The id of the Manager Widget.
	manager_widget:manager_widget, $

	; Handles for local mask.
	filter_mask_ptr: ptr_new(/ALLOC), $


	;IDs of widgets that generate events or have to be updated.
	;Many of these widgets have "value" slots that hold state information.

	top_base:top_base, button_base:button_base,  $
	entry_list_widget:entry_list_widget, $
	accept_button:accept_button, reject_button:reject_button, $
	default1_button:default1_button, default2_button:default2_button, default3_button:default3_button, $
	destroy_button:destroy_button, $


	; Other state information.
	entry_list:entry_list, selected_entry:0, $
	compute_mask_flag:1 }

widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Realize the widget and register with manager.
widget_control, top_base, /REALIZE

;; Simulate revision of the filter to generate an event for the manager widget.
AcisGradeFilterEvent, {ID:button_base, TOP:button_base, HANDLER:top_base}

print, 'See http://www.astro.psu.edu/xray/docs/TARA/TARA_users_guide/TARA_users_guide.html Section 3.3.6 for ACIS Grade definitions.'

xmanager, 'ACIS grade filter', top_base, GROUP_LEADER=group, $
	  EVENT_HANDLER='AcisGradeFilterEvent', /NO_BLOCK, $
	  CLEANUP='AcisGradeFilterCleanup'
	  
return, top_base
end

