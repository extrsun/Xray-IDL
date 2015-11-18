;+
;==========================================================================
;;;
;;; FILE NAME:    $Id: rowcolumn_filter.pro 610 1998-08-11 13:17:59Z patb $
;;;
;;; DESCRIPTION:  A row and column filter for selecting
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1997, Pennsylvania State University
;;;
;;; NOTES:        See the document "X-ray Event Browser Software Design
;;;               Specification"
;;;
;;;               The manager_widget parameter is the widget ID of the
;;;               Manager Widget (the widget that is calling this function).
;;; 
;;;               The UVALUE of the first child of the base is used to store
;;;               a structure representing the state of the filter widget.
;;;               In this state is an IDL handle that is used to store the
;;;               byte mask array for the filter, which form the VALUE
;;;               property of the widget itself.
;;;
;-
;==========================================================================
;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO RowColumnFilterCleanup, top_base

widget_control, top_base, GET_UVALUE=state, /NO_COPY


;; Send an event to the manager.
new_event = $
	 { FILTER_REVISED, ID:state.manager_widget, TOP:state.manager_widget, $
	   HANDLER:0L, NAME:'filter_revised', FILTER_ID:state.top_base, $
	   DESCRIPTION: 'defunct filter' }

widget_control, state.manager_widget, SEND_EVENT=new_event, BAD_ID=bad_id

return
end

 

;==========================================================================
PRO RowColumnFilterEvent, event
 
filter_revised = 0
destroy_flag   = 0
redisplay_flag = 0
 
;; Get the widget state structure.
top_base = event.handler
widget_control, top_base,  GET_UVALUE=state, /NO_COPY, $
		/HOURGLASS
 
 
;; Handle the event; branch first based on which widget generated it.
case event.id of
 
  ;------------------------------------------------------------------------
 state.top_base: $
   begin
   case event.name of

      ;; Filters are being modified.  No working dataset is available.
     'working_set_stale': 
 
     'working_set_delete': 

      ;; The working dataset has been recomputed.  
     'working_set_fresh': 
 
      ;; The domain dataset has been revised.  Get the new data.
     'domain_set_revised' : state.compute_mask_flag = 1
 
     else: message, 'event not handled'
   endcase
   end
 
  ;------------------------------------------------------------------------
  ; This is used only for widget initialization.
  state.label_widget: filter_revised = 1
  
  ;------------------------------------------------------------------------
  state.rowcolumn_buttons:
    ;do nothing we'll read the value of this widget when needed

  ;------------------------------------------------------------------------
  state.adddel_buttons: $
    begin

      ; Get the list of values
      handle_value,state.val_handle,rc_list_values,/NO_COPY
      handle_value,state.type_handle,rc_list_types,/NO_COPY

      rc=['row','col']

      case event.value of

        ; "Add" button pressed
        0: $
	begin
          filter_revised = 1
	  state.compute_mask_flag = 1

          ; Get the value to add to the list
          widget_control,state.field_widget,GET_VALUE=val

	  ; Is is a row or a column?
          widget_control,state.rowcolumn_buttons,GET_VALUE=type
	  type = rc(type)

          ; If the first value on the list is INFINITY, then overwrite it
	  if( rc_list_values(0) EQ 2147483647 ) then begin
            rc_list_values    = fltarr(1)
            rc_list_types     = strarr(1)
            rc_list_values(0) = val
            rc_list_types(0)  = type
	  endif else begin
	  ; Otherwise, add a row/column to the list.
	    s = size(rc_list_values)
	    new_values = fltarr(s(1) + 1)
	    new_types  = strarr(s(1) + 1)
	    new_values(0:s(1)-1) = rc_list_values
	    new_types(0:s(1)-1)  = rc_list_types
	    new_values(s(1)) = val
	    new_types(s(1))  = type
	    rc_list_values = new_values
	    rc_list_types  = new_types
	  endelse
	  state.list_item_selected = -1
	end

        ; "Delete" button pressed
        1: $
	begin
	  if (state.list_item_selected NE -1) then begin
	    state.compute_mask_flag = 1
            filter_revised = 1
	    index = state.list_item_selected
	    s = size( rc_list_values )

            if (s(1) EQ 1) then begin
	      rc_list_values(0) = 2147483647
	      rc_list_types(0) = 'row'
	    endif else begin
              newvals  = fltarr(s(1) - 1)
              newtypes = strarr(s(1) - 1)
	      newindex = 0
	      for ii=0, s(1)-1 do begin
	        if (ii NE index) then begin
		newvals(newindex)    = rc_list_values(ii)
		newtypes(newindex)   = rc_list_types(ii)
		newindex   = newindex + 1
	        endif
	      endfor
	      rc_list_values = newvals
	      rc_list_types  = newtypes
	    endelse
	    state.list_item_selected = -1
	  endif else begin
	    str = strarr(3)
	    str(0) = 'You must first select an entry in the list'
	    str(1) = 'of rows and columns to ignore.  Do this by'
	    str(2) = 'clicking on the appropriate list entry.'
	    result = dialog_message( str, DIALOG_PARENT=state.top_base )
	  endelse
	end

        ; "Dismiss" button pressed
        2: $
	begin
          filter_revised = 1
          destroy_flag=1
	end

	else: $
	begin
	  message,'Invalid event value.'
        end

        endcase

      ; Display the new values in of rows/columns to filter out
      FormatRowColumnList, rc_list_values, rc_list_types, list
      widget_control,state.list_widget,SET_VALUE=list

      ; Put back the list of values
      handle_value,state.val_handle,rc_list_values, /SET, /NO_COPY
      handle_value,state.type_handle,rc_list_types, /SET, /NO_COPY
    end

  ;------------------------------------------------------------------------
 state.field_widget: $
   begin
     widget_control,state.field_widget,GET_VALUE=new_value
   end

  ;------------------------------------------------------------------------
  state.list_widget: $ 
    state.list_item_selected = event.index

  ;------------------------------------------------------------------------
 else: message, 'event not handled'
endcase


; If the filter mask was revised, create an event for the Manager Widget.
if (filter_revised) then begin

  new_event = $
  { FILTER_REVISED, ID:state.manager_widget, TOP:state.manager_widget, $
    HANDLER:0L, NAME:'filter_revised', FILTER_ID:state.top_base, $
    DESCRIPTION: 'row/column filter' }

  widget_control, state.manager_widget, SEND_EVENT=new_event

endif

;; Save the state structure.
rowcolumn_filter_handle = state.rowcolumn_filter_handle
widget_control, top_base,  SET_UVALUE=state, /NO_COPY

;;; Destroy the widget if desired.
if (destroy_flag) then begin
  handle_free, rowcolumn_filter_handle
  widget_control, top_base, /DESTROY
endif 

return
end

;==========================================================================
FUNCTION GetRowColumnFilterValue, top_base

;; Get the widget state structure.
 widget_control, top_base,  GET_UVALUE=state, /NO_COPY

;; If necessary, recompute the filter mask.
if (state.compute_mask_flag) then begin
 state.compute_mask_flag = 0

 ; Retrieve the event locations
 z=GetProperty('y_position', DDS_DATA=eventrows)
 z=GetProperty('x_position', DDS_DATA=eventcols)

 ; Determine which rows and columns are to be filtered
 handle_value,state.val_handle,rc_list_values,/NO_COPY
 handle_value,state.type_handle,rc_list_types,/NO_COPY

 mask = *eventrows EQ *eventrows

 for ii=0, n_elements(rc_list_values)-1 do begin
   if( rc_list_values(ii) EQ 2147483647 ) then begin
     ; do nothing.  The list is empty.
   endif else begin
     if (rc_list_types(ii) EQ 'row') then begin
       mask = mask AND (*eventrows NE rc_list_values(ii))
     endif else if (rc_list_types(ii) EQ 'col') then begin
       mask = mask AND (*eventcols NE rc_list_values(ii))
     endif else begin
       message,'Bad type found in row/column list'
     endelse
   endelse
 endfor

 handle_value,state.val_handle,rc_list_values,/SET,/NO_COPY
 handle_value,state.type_handle,rc_list_types,/SET,/NO_COPY

 ;; Store the mask and return the ampltude vector.
 handle_value, state.filter_mask_handle, mask, /SET, /NO_COPY
endif

 handle_value, state.filter_mask_handle, filter_mask

;;Save the state structure.
widget_control, top_base,  SET_UVALUE=state, /NO_COPY

return, filter_mask
end

PRO FormatRowColumnList, rc_list_values, rc_list_types, list 

  s = size(rc_list_values)
  if( rc_list_values(0) EQ 2147483647) then begin
    list = 'Accept all rows and columns'
  endif else begin
    list =  rc_list_types + ':' + string(rc_list_values)
  endelse
return
end

;==========================================================================
FUNCTION rowcolumn_filter, manager_widget, manager_widget_handle, $
         UVALUE = uvalue, TITLE=title, GROUP=group
 
if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'Row/Column Filter'
if NOT keyword_set(group)  then group  = 0

;; Initialize handles to hold data and mask.
  bins = CountXrayData()
  if (bins EQ 0) then begin
    message, 'no data bins returned by CountXrayData()'
  endif 

  rowcolumn_filter_handle = handle_create( manager_widget_handle )

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.
 
  top_base = widget_base( UVALUE=uvalue, TITLE=title, GROUP=group, /COLUMN, $
                          FUNC_GET_VALUE='GetRowColumnFilterValue', $
                          /BASE_ALIGN_CENTER, XOFFSET=0, YOFFSET=270 )
 
  frame_widget0 = widget_base( top_base, /COLUMN )
  frame_widget1 = widget_base( frame_widget0, /FRAME, /COLUMN)
    label_widget = widget_label( frame_widget1, /ALIGN_LEFT, $
				 VALUE='Rows/Columns to ignore:' )

    list_widget = widget_list( frame_widget1, $
			       VALUE= 'Accept all rows and columns', $
			       YSIZE=10, XSIZE=45 )

  frame_widget2 = widget_base( frame_widget0, /FRAME, /ROW )
    field_widget = cw_field ( frame_widget2, /INTEGER, VALUE=0, XSIZE=10, $
			      /RETURN_EVENTS, $
			      TITLE='Add Row/Column:' )

    rowcolumn_buttons = cw_bgroup( frame_widget2, ['Row','Column'], /ROW, $
				   /EXCLUSIVE,SET_VALUE=0)


  frame_widget3 = widget_base( frame_widget0, /FRAME, /COLUMN )
    adddel_buttons = cw_bgroup( frame_widget3, /ROW, $
		          ['Add To List','Delete From List','Dismiss Filter'] )

  values = fltarr(1)
  values(0)=2147483647
  types  = strarr(1)
  types(0) = 'row'
  val_handle=handle_create(rowcolumn_filter_handle)
  type_handle=handle_create(rowcolumn_filter_handle)
  handle_value,val_handle,values,/SET
  handle_value,type_handle,types,/SET

  state = { $
        ; The id of the Manager Widget.
        manager_widget:manager_widget, $
 
	rowcolumn_filter_handle:rowcolumn_filter_handle, $

        ; Handles for x-ray data, local mask, and composite mask.
        filter_mask_handle: handle_create(rowcolumn_filter_handle), $
 
 
        ;IDs of widgets that generate events or have to be updated.
        ;Many of these widgets have "value" slots that hold state information.
 
        top_base:top_base, label_widget:label_widget, $
	rowcolumn_buttons:rowcolumn_buttons,$
	field_widget:field_widget,$
	adddel_buttons:adddel_buttons,$
	list_widget:list_widget,$
 
        ; Other state information.
	val_handle:val_handle, $
	type_handle:type_handle, $
	list_item_selected:-1, $
	compute_mask_flag:1 }
 
widget_control, top_base,  SET_UVALUE=state, /NO_COPY

;; Realize the widget.
widget_control, top_base, /REALIZE

;; Simulate revision of the filter to generate an event for the manager widget.
RowColumnFilterEvent, {ID:label_widget, TOP:label_widget, HANDLER:top_base}


;; Register with the manager widget.
xmanager, 'generic filter', top_base, GROUP_LEADER=group, $
          EVENT_HANDLER='RowColumnFilterEvent', /NO_BLOCK, $
          CLEANUP='RowColumnFilterCleanup'
 
return, top_base
end

