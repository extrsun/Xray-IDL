;+
;========================================================================
;;;
;;; FILE NAME:    $Id: eb_dataset_1d.pro 1599 2002-06-20 15:32:50Z patb $
;;;
;;; DESCRIPTION:  Univariate Analysis Widget for Event Browser
;;;
;;;               The manager_widget parameter is the widget ID of the 
;;;               Manager Widget (the widget that is calling this function).
;;;
;;;               The UVALUE of the first child of the base is used to store
;;;               a structure representing the state of the display widget.  
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
;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO EbDataset1dCleanup, top_base

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
FUNCTION EbDataset1dDisplayEvent, event

destroy_flag      = 0
new_property_flag = 0
filter_revised    = 0
redisplay_flag    = 0
dds_revised       = 0

;; Get the widget state structure.
top_base = event.handler
widget_control, top_base, GET_UVALUE=state, /NO_COPY, /HOURGLASS


;; Handle the event; branch first based on which widget generated it.
case event.id of

  ;------------------------------------------------------------------------
  state.top_base: $
    begin
    case event.name of
      'working_set_stale': $
	begin
	end

      'working_set_delete': $
	begin
        dataset_1d, state.dataset_1d, /DELETE, REDRAW=0, $
		    DATASET_NAME=state.dataset_name
	end

      'working_set_fresh' : $
	begin
	state.dataset_name = event.dataset_name
        redisplay_flag     = 1
	end

      'domain_set_revised' : $
	begin
        dds_revised        = 1
        state.relabel_flag       = 1
	end

      else: message, 'event not handled'
    endcase
    end


  ;------------------------------------------------------------------------
  state.horizontal_property: $
    begin
    state.hprop_name = state.prop_list[event.index]
    new_property_flag = 1
    dds_revised       = 1
    redisplay_flag    = 1
    state.relabel_flag      = 1
    end

  ;------------------------------------------------------------------------
  state.dataset_1d: $
    begin
    filter_revised = 1
    end

  ;------------------------------------------------------------------------
  state.destroy_button: $
    begin
    destroy_flag   = 1
    filter_revised = 1
    end

  ;------------------------------------------------------------------------
  else: message, 'event not handled'
endcase


if (redisplay_flag) then begin
  z=GetProperty( state.hprop_name, WDS_DATA=hdata, LABEL=hlabel, WCS=hwcs )
  state.hlabel = hlabel
  
  if (new_property_flag) then begin
    case state.hprop_name of
     'pha': 		binsize = 1.0
     'energy': 		binsize = 10.0
     'instrument_grade': 	binsize = 1.0
     'exposure': 		binsize = 1.0
     else: binsize = 0
    endcase
     
    dataset_1d, state.dataset_1d, DATASET=state.dataset_name, $
    		REDRAW=0, BINSIZE=binsize
  endif
  
  if (state.relabel_flag) then begin        
    state.relabel_flag = 0   
    dataset_1d, state.dataset_1d, hdata, XTITLE=hlabel, XWCS=hwcs, $
		DATASET_NAME=state.dataset_name, DESCRIPT=state.dataset_name 
  endif else begin
    dataset_1d, state.dataset_1d, hdata, $
		 DATASET_NAME=state.dataset_name, DESCRIPT=state.dataset_name 
  endelse
endif ;redisplay_flag SET


if (dds_revised) then begin
  z=GetProperty( state.hprop_name, DDS_DATA=hdata )

  dataset_1d, state.dataset_1d, *hdata, /DISABLE_DELETE, /HIDE_DATASET, $
	      BINSIZE=0, $
	      DATASET_NAME='domain dataset', DESCRIPTION='domain dataset'
endif ;dds_revised SET


if (filter_revised) then begin
  new_event = $
	 { FILTER_REVISED, ID:state.manager_widget, TOP:state.manager_widget, $
	   HANDLER:0L, NAME:'filter_revised', FILTER_ID:state.top_base, $
	   DESCRIPTION: state.hlabel + ' filter'}

  widget_control, state.manager_widget, SEND_EVENT=new_event
endif



;; Save the state structure.
parent = state.parent
widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Destroy the widget if desired.
if (destroy_flag) then widget_control, parent, /DESTROY

return, 0
end


;==========================================================================
FUNCTION GetEbDataset1dValue, top_base

;; Get the widget state structure.
widget_control, top_base, GET_UVALUE=state, /NO_COPY

  
  ;; Apply the ROI 'DDS filter' to the dataset 'domain dataset'.
  dataset_1d, state.dataset_1d, DATASET_NAME='domain dataset', ROI_MASK=roi_mask

  ;; A disabled filter is represented by *roi_mask undefined.
  if (n_elements( *roi_mask ) EQ 0) then begin
    mask = 0
  endif else begin
    mask = *roi_mask

    dum = where( mask, count )
  endelse

;; Save the state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY
 
return, mask
end


;========================================================================
FUNCTION eb_dataset_1d, manager_widget, dataset_name, hprop_name, $
	 UVALUE=uvalue, TITLE=title, GROUP=group, GENERIC=generic

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'Univariate Analysis'
if NOT keyword_set(group)  then group  = 0


;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.
sz = get_screen_size()

parent = plot_window_topbase(UVALUE=uvalue, TITLE=title, GROUP=group,$
			     XOFFSET=sz[0]-680, YOFFSET=sz[1]-640)

top_base = widget_base( parent, /COLUMN, $
			FUNC_GET_VALUE='GetEbDataset1dValue', $
			EVENT_FUNC='EbDataset1dDisplayEvent', $
			KILL_NOTIFY='EbDataset1dCleanup')


 dataset_1d_base = widget_base( top_base, /SPACE, XPAD=0, YPAD=0 )

 dataset_1d, dataset_1d, PARENT_WIDGET=dataset_1d_base

 bottom_base = widget_base( top_base, /ROW, SPACE=10, /ALIGN_RIGHT )

 if keyword_set(generic) then begin
   prop_list = [GetPropertyNames( /FITS, DIM=1, /NUMERIC_ONLY ),'timestamp']
 endif else begin
   prop_list = ['x_position', 'y_position', 'timestamp', 'energy', 'wavelength', 'pha', 'instrument_grade', 'exposure', 'generic1', 'generic2', 'generic3','generic4', 'generic5', 'generic6']
 endelse

  horizontal_property = widget_droplist( bottom_base, VALUE=prop_list, $
  					 TITLE='Property' )

  horizontal_index = (where(prop_list EQ hprop_name, count))[0]
  if (count EQ 0) then begin
    horizontal_index = 0
    hprop_name = prop_list[horizontal_index]
  endif
  
  widget_control, horizontal_property, SET_DROPLIST_SELECT=horizontal_index

  destroy_button = widget_button( bottom_base,VALUE='Dismiss' )


;; Create the widget state structure.

state = { parent:parent, $ 
	; The id of the Manager Widget.
	manager_widget:manager_widget, $

	;IDs of widgets that generate events or have to be updated.
	top_base:top_base, dataset_1d_base:dataset_1d_base,$
	dataset_1d:dataset_1d, prop_list:prop_list, hprop_name:hprop_name, $
	horizontal_property:horizontal_property, $
	destroy_button:destroy_button, $

	; Other stuff
	hlabel:'', dataset_name: dataset_name, relabel_flag:0 }


;; Realize the widget and register with manager.
widget_control, parent, /REALIZE

;; Simulate an event to initially draw the display.
event = {ID:horizontal_property, TOP:horizontal_property, $
	 HANDLER:0L, INDEX:horizontal_index}
widget_control, horizontal_property, SEND_EVENT=event

xmanager, 'EbDataset1d', parent, GROUP_LEADER=group, $
	  EVENT_HANDLER='PlotWindowTopbaseEventFn', /NO_BLOCK
	  
;; Store the state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

return, top_base
end


