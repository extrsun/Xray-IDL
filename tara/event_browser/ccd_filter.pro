;+
;==========================================================================
;;;
;;; FILE NAME:     $Id: ccd_filter.pro 874 1999-09-12 20:11:37Z patb $
;;;
;;; DESCRIPTION:  CCD Filter Widget for X-ray Event Browser application.
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
PRO CcdFilterCleanup, top_base

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
PRO CcdFilterEvent, event

filter_revised = 0
destroy_flag   = 0
redisplay_flag = 0
generate_entry_list_flag = 0

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
        generate_entry_list_flag = 1
	end

      else: message, 'event not handled'
    endcase
    end

  ;------------------------------------------------------------------------
  ; These events used only for widget initialization.
  state.button_base: $
    begin
    generate_entry_list_flag = 1
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
    index = where( state.entry_list.status NE -1, count )
    if (count GT 0) then state.entry_list[index].status = 1
    filter_revised = 1
    end
    
  ;------------------------------------------------------------------------
  state.reject_button: $
    begin
    index = where( state.entry_list.status NE -1, count )
    if (count GT 0) then state.entry_list[index].status = 0
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
      DESCRIPTION: 'CCD/AMP filter' }

  widget_control, state.manager_widget, SEND_EVENT=new_event

  redisplay_flag          = 1
  state.compute_mask_flag = 1
endif 


;; If the domain dataset has been changed, we need to re-generate the 
;; entry_list array to reflect the (CCD,AMP) combinations actually found
;; in the domain dataset.
if (generate_entry_list_flag) then begin

  ; Get the new x-ray data.
  z=GetProperty( 'ccd_id', DDS_DATA=ccds )
  z=GetProperty( 'amp_id', DDS_DATA=amps )


  ; For each (CCD,AMP) combination, make sure we have an entry in entry_list.
  unprocessed_flags = replicate( 1B, n_elements(*ccds) )
  next_event = 0

  repeat begin
    ; Define a set of events that share a (CCD,AMP) combination and mark that
    ; set as processed.
    ccd   = (*ccds)[next_event]    
    amp   = (*amps)[next_event]
    index = where( (*ccds EQ ccd) AND (*amps EQ amp), event_count )
    unprocessed_flags(index) = 0

    ; Create an entry in entry_list if necessary.
    index = where( (state.entry_list.status NE -1)    AND $
		   (state.entry_list.ccd    EQ ccd)   AND $
		   (state.entry_list.amp    EQ amp), count )

    if (count EQ 0) then begin
      index = where( state.entry_list.status EQ -1 )
      new_entry = index(0)
      state.entry_list(new_entry).ccd    = ccd
      state.entry_list(new_entry).amp    = amp
      state.entry_list(new_entry).status = 0
    endif
    

    ; Set up for next loop iteration.
    next_event = where( unprocessed_flags, unprocessed_count )
    next_event = next_event(0)
  endrep until (unprocessed_count EQ 0)

  ;; Sort the entry list by CCD.
  entry_list  = state.entry_list
  srt_index = sort( entry_list.ccd )
  state.entry_list = entry_list( srt_index )
endif


;; If necessary, redraw the entry list widget.
if (redisplay_flag) then begin
  entry_list  = state.entry_list
  num_entries = n_elements(entry_list)

  text = strarr( num_entries )

  for ii=0, num_entries-1 do begin
    entry = entry_list(ii)

    label = string( entry.ccd, entry.amp, $
		    f='("CCD ",I0,", AMP ",I0," :")' )

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
widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Destroy the widget if desired.
if (destroy_flag) then widget_control, top_base, /DESTROY

return
end


;==========================================================================
FUNCTION GetCcdFilterValue, top_base

;; Get the widget state structure.
widget_control, top_base, GET_UVALUE=state, /NO_COPY

;; If necessary, recompute the filter mask.
if (state.compute_mask_flag) then begin
  state.compute_mask_flag = 0

  ; Get the new x-ray data.
  z=GetProperty( 'ccd_id', DDS_DATA=ccds )
  z=GetProperty( 'amp_id', DDS_DATA=amps )


  ; Compute the mask vector.
  mask = replicate( 1B, CountXrayData() > 1 )

  entry_list = state.entry_list
  for ii=0, n_elements(entry_list)-1 do begin
    entry = entry_list(ii)

    if (entry.status EQ 0) then begin
      index = where( (*ccds EQ entry.ccd) AND (*amps EQ entry.amp), event_count )

      if (event_count GT 0) then mask(index) = 0
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
FUNCTION ccd_filter, manager_widget, $
	 UVALUE = uvalue, TITLE=title, GROUP=group 

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'CCD/AMP Filter'
if NOT keyword_set(group)  then group  = 0


;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

top_base = widget_base( UVALUE=uvalue, TITLE=title, GROUP=group, /COLUMN, $
	                FUNC_GET_VALUE='GetCcdFilterValue', $
			/BASE_ALIGN_CENTER, XOFFSET=0, YOFFSET=370 )

 entry_list_widget = widget_list( top_base, XSIZE=23, YSIZE=25 )

 button_base = widget_base( top_base, /ROW, /ALIGN_RIGHT )

 accept_button = widget_button( top_base, VALUE='ACCEPT ALL' )

 reject_button = widget_button( top_base, VALUE='REJECT ALL' )

 destroy_button = widget_button( top_base, VALUE='Dismiss Filter' )


;; Initialize data structures associated with entry list widget.
;; Status values have the following meanings:
;;  -1: there is no entry at this location in the entry_list array
;;   0: the entry is REJECTED by the user
;;   1: the entry is ACCEPTED by the user

MAX_ENTRIES = 40
entry={ccd: 99, amp:0B, status:-1, domain_event_count:0L }
entry_list = replicate( entry, MAX_ENTRIES )


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
	destroy_button:destroy_button, $


	; Other state information.
	entry_list:entry_list, selected_entry:0, $
	compute_mask_flag:1 }

widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Realize the widget and register with manager.
widget_control, top_base, /REALIZE

;; Simulate revision of the filter to generate an event for the manager widget.
CcdFilterEvent, {ID:button_base, TOP:button_base, HANDLER:top_base}



xmanager, 'ccd filter', top_base, GROUP_LEADER=group, $
	  EVENT_HANDLER='CcdFilterEvent', /NO_BLOCK, $
	  CLEANUP='CcdFilterCleanup'
	  
return, top_base
end

