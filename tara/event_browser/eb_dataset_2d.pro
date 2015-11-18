;+
;========================================================================
;;;
;;; FILE NAME:    $Id: eb_dataset_2d.pro 1599 2002-06-20 15:32:50Z patb $
;;;
;;; DESCRIPTION:  Bivariate Analysis Widget for Event Browser
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
PRO EbDataset2dCleanup, top_base

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
FUNCTION EbDataset2dDisplayEvent, event

destroy_flag      = 0
filter_revised    = 0
redisplay_flag    = 0

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
      'working_set_stale': $
	begin
	end

      'working_set_delete': $
	begin
        dataset_2d, state.dataset_2d, /DELETE, REDRAW=0, $
        	    DATASET_NAME=state.dataset_name
        end
          	    
      'working_set_fresh' : $
	begin
	state.dataset_name = event.dataset_name
        redisplay_flag     = 1
	end

      'domain_set_revised' : $
	begin
        state.dds_revised        = 1
        state.relabel_flag       = 1
	end

      else: message, 'event not handled'
    endcase
    end


  ;------------------------------------------------------------------------
  state.rot_angle: $
    begin
    state.dds_revised       = 1
    redisplay_flag          = 1
    state.relabel_flag      = 1
    end

  ;------------------------------------------------------------------------
  state.horizontal_property: $
    begin
    state.hprop_name = state.prop_list[event.index]
    state.new_property_flag = 1
    state.dds_revised       = 1
    state.relabel_flag      = 1
    end

  ;------------------------------------------------------------------------
  state.vertical_property: $
    begin
    state.vprop_name = state.prop_list[event.index]
    state.new_property_flag = 1
    state.dds_revised       = 1
    state.relabel_flag      = 1
    end

  ;------------------------------------------------------------------------
  state.dataset_2d: $
    begin
    filter_revised = 1
    end

  ;------------------------------------------------------------------------
  state.replot_button: $
    begin
    redisplay_flag = 1
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

widget_control, state.rot_angle, GET_VALUE=angle_deg
angle_rad = angle_deg * !PI / 180.0

if (state.new_property_flag) then begin
  ;; Hide the plot, and enable the replot button.
  widget_control, state.dataset_2d,    SENSITIVE=0, BAD_ID=bad_id
  widget_control, state.replot_button, SENSITIVE=1
endif


if (redisplay_flag) then begin
  z=GetProperty( state.hprop_name, WDS_DATA=hdata, LABEL=hlabel, WCS=hwcs )
  state.hlabel = hlabel
  z=GetProperty( state.vprop_name, WDS_DATA=vdata, LABEL=vlabel, WCS=vwcs )
  state.vlabel = vlabel
    
  if (angle_rad NE 0.0) then begin
    vlabel=vlabel+string(angle_deg,f='(", rotated by ",I0," degrees")')
    hlabel=hlabel+string(angle_deg,f='(", rotated by ",I0," degrees")')
    H=temporary(hdata)
    V=temporary(vdata)
    hdata = H * cos(angle_rad) - V * sin(angle_rad)
    vdata = H * sin(angle_rad) + V * cos(angle_rad)
  endif

  if (state.new_property_flag) then begin
    state.new_property_flag = 0
    dataset_2d, state.dataset_2d, DATASET_NAME=state.dataset_name, $
    		REDRAW=0, XBIN=0, YBIN=0
  endif
  
  if (state.relabel_flag) then begin 
    state.relabel_flag = 0   
    dataset_2d, state.dataset_2d, hdata, vdata,  $
		 XTITLE=hlabel, YTITLE=vlabel, $
		 XWCS=hwcs, YWCS=vwcs, $
		 UNITY_ASPECT=(state.hprop_name EQ 'x_position') AND $
		 	      (state.vprop_name EQ 'y_position'), $
		 DATASET_NAME=state.dataset_name, DESCRIPT=state.dataset_name 
  endif else begin
    dataset_2d, state.dataset_2d, hdata, vdata, $
		 DATASET_NAME=state.dataset_name, DESCRIPT=state.dataset_name 
  endelse
  
  widget_control, state.dataset_2d,    SENSITIVE=1
  widget_control, state.replot_button, SENSITIVE=0
endif ;redisplay_flag SET


if (state.dds_revised) then begin
  state.dds_revised = 0
  z=GetProperty( state.hprop_name, DDS_DATA=hdata_ptr )
  z=GetProperty( state.vprop_name, DDS_DATA=vdata_ptr )

  if (angle_rad NE 0.0) then begin
    hdata = (*hdata_ptr) * cos(angle_rad) - (*vdata_ptr) * sin(angle_rad)
    vdata = (*hdata_ptr) * sin(angle_rad) + (*vdata_ptr) * cos(angle_rad)
    dataset_2d, state.dataset_2d, hdata, vdata, /DISABLE_DEL, /HIDE_DATASET,$
	       DATASET_NAME='domain dataset', DESCRIPTION='domain dataset'
  endif else begin
    dataset_2d, state.dataset_2d, *hdata_ptr, *vdata_ptr, $
    		/DISABLE_DEL, /HIDE_DATASET,$
	        DATASET_NAME='domain dataset', DESCRIPTION='domain dataset'
  endelse
endif ;state.dds_revised SET


if (filter_revised) then begin
  new_event = $
	 { FILTER_REVISED, ID:state.manager_widget, TOP:state.manager_widget, $
	   HANDLER:0L, NAME:'filter_revised', FILTER_ID:state.top_base, $
	   DESCRIPTION: state.hlabel + '/' + state.vlabel + ' filter'}

  widget_control, state.manager_widget, SEND_EVENT=new_event
endif



;; Save the state structure.
parent = state.parent
widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Destroy the widget if desired.
if (destroy_flag) then widget_control, parent, /DESTROY

return,0
end


;==========================================================================
FUNCTION GetEbDataset2dValue, top_base

;; Get the widget state structure.
widget_control, top_base, GET_UVALUE=state, /NO_COPY

  
  ;; Apply the ROI 'DDS filter' to the dataset 'domain dataset'.
  dataset_2d, state.dataset_2d, DATASET_NAME='domain dataset', ROI_MASK=roi_mask

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
FUNCTION eb_dataset_2d, manager_widget, $
	 dataset_name, hprop_name, vprop_name, $
	 UVALUE=uvalue, TITLE=title, GROUP=group, GENERIC=generic

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'Bivariate Analysis'
if NOT keyword_set(group)  then group  = 0

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.
sz = get_screen_size()

parent = plot_window_topbase(UVALUE=uvalue, TITLE=title, GROUP=group,$
			     XOFFSET=sz[0]-680, YOFFSET=sz[1]-680)


top_base = widget_base( parent, /COLUMN, $
			FUNC_GET_VALUE='GetEbDataset2dValue', $
			EVENT_FUNC='EbDataset2dDisplayEvent', $
			KILL_NOTIFY='EbDataset2dCleanup')

 dataset_2d_base = widget_base( top_base, /SPACE, XPAD=0, YPAD=0 )

 dataset_2d, dataset_2d, PARENT_WIDGET=dataset_2d_base

 bottom_base = widget_base( top_base, /ROW, SPACE=100,XPAD=0,YPAD=0, /ALIGN_LEFT )
 left_base   = widget_base( bottom_base, ROW=2, /SPACE,XPAD=0,YPAD=0 )

 if keyword_set(generic) then begin
   prop_list = [GetPropertyNames( /FITS, DIM=1, /NUMERIC_ONLY ),'timestamp']
 endif else begin
   prop_list = ['x_position', 'y_position', 'timestamp', 'energy', 'wavelength', 'pha', 'instrument_grade', 'asca_grade', 'exposure', 'generic1', 'generic2', 'generic3','generic4', 'generic5', 'generic6']
 endelse


  vertical_property = $
	widget_droplist( left_base, VALUE=prop_list, TITLE='Vertical' )

  dum = widget_label(left_base, VALUE=' ')
  
  rot_angle = cw_field( left_base, /FLOAT, /RETURN_EVENTS, $
			VALUE=0.0, TITLE='Rotation (degrees)', XSIZE=4)

  horizontal_property = $
	widget_droplist( left_base, VALUE=prop_list, TITLE='Horizontal' )

  horizontal_index = (where(prop_list EQ hprop_name, count))[0]
  if (count EQ 0) then begin
    horizontal_index = 0
    hprop_name = prop_list[horizontal_index]
  endif
  
  vertical_index   = (where(prop_list EQ vprop_name, count))[0]
  if (count EQ 0) then begin
    vertical_index = 1 < (n_elements(prop_list)-1)
    vprop_name = prop_list[vertical_index]
  endif
  
  widget_control, vertical_property,   SET_DROPLIST_SELECT=vertical_index
  widget_control, horizontal_property, SET_DROPLIST_SELECT=horizontal_index

			
 right_base = widget_base( bottom_base, /ROW, SPACE=5,XPAD=0,YPAD=0, /ALIGN_RIGHT )
  replot_button = widget_button(right_base,VALUE='Replot')
  destroy_button = widget_button( right_base,VALUE='Dismiss' )

;; Create the widget state structure.

state = { parent:parent, $
	; The id of the Manager Widget.
	manager_widget:manager_widget, $

	;IDs of widgets that generate events or have to be updated.
	top_base:top_base, dataset_2d_base:dataset_2d_base,$
	dataset_2d:dataset_2d, rot_angle:rot_angle, $
	prop_list:prop_list, $
	hprop_name:hprop_name, vprop_name:vprop_name,$
	horizontal_property:horizontal_property, $
	vertical_property:vertical_property,$
	replot_button:replot_button, destroy_button:destroy_button, $

	; Other stuff
	hlabel:'', vlabel:'', $
	dataset_name: dataset_name, $
	new_property_flag:1, dds_revised:1, relabel_flag:1 }


;; Realize the widget and register with manager.
widget_control, parent, /REALIZE

;; Simulate an event to initially draw the display.
event = {ID:replot_button, TOP:replot_button, $
	 HANDLER:0L, INDEX:replot_button}
widget_control, replot_button, SEND_EVENT=event

xmanager, 'EbDataset2d', parent, GROUP_LEADER=group, $
	  EVENT_HANDLER='PlotWindowTopbaseEventFn', /NO_BLOCK
	  
;; Store the state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

return, top_base
end


