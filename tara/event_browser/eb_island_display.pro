;+
;========================================================================
;;;
;;; FILE NAME:    $Id: eb_island_display.pro 1154 2000-07-17 09:23:33Z patb $
;;;
;;; DESCRIPTION:  Event Island Analysis Widget for Event Browser
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
FUNCTION EbIslandDisplayEvent, event

destroy_flag      = 0
redisplay_flag    = 0

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
	if state.map_mode then begin
          dataset_3d, state.dataset_3d, /DELETE, REDRAW=0, $
		    DATASET_NAME=state.dataset_name
        endif else begin
          dataset_1d, state.dataset_1d, /DELETE, REDRAW=0, $
		    DATASET_NAME=state.dataset_name
	endelse
	end

      'working_set_fresh' : $
	begin
	state.dataset_name = event.dataset_name
        redisplay_flag     = 1
	end

      'domain_set_revised' : $
	begin
        state.relabel_flag       = 1
	end

      else: message, 'event not handled'
    endcase
    end


  ;------------------------------------------------------------------------
  state.dataset_1d: 
  state.dataset_3d: 

  ;------------------------------------------------------------------------
  state.position_buttons: $
    begin
    if state.map_mode then begin
      widget_control, state.dataset_3d, SENSITIVE=0
    endif else begin
      widget_control, state.dataset_1d, SENSITIVE=0
    endelse
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
    end

  ;------------------------------------------------------------------------
  else: message, 'event not handled'
endcase


if (redisplay_flag) then begin
  if state.map_mode then begin
    widget_control, state.dataset_3d, SENSITIVE=1
  endif else begin
    widget_control, state.dataset_1d, SENSITIVE=1
  endelse

  dataset_name = ''

  widget_control, state.position_buttons, GET_VALUE=button_flags

  list_control, pix_list, /CREATE

  num_buttons = n_elements(button_flags)
  data_found  = bytarr(num_buttons)
  for ii=0, num_buttons-1 do begin
    if (button_flags[ii]) then begin
      case ii of
      2: data_found[ii]=GetProperty('ll_pix',	WDS_DATA=pixels)
      5: data_found[ii]=GetProperty('down_pix',	WDS_DATA=pixels)
      8: data_found[ii]=GetProperty('lr_pix',	WDS_DATA=pixels)
      1: data_found[ii]=GetProperty('left_pix',	WDS_DATA=pixels)
      4: data_found[ii]=GetProperty('center_pix',	WDS_DATA=pixels)
      7: data_found[ii]=GetProperty('right_pix',	WDS_DATA=pixels)
      0: data_found[ii]=GetProperty('ul_pix',	WDS_DATA=pixels)
      3: data_found[ii]=GetProperty('up_pix',	WDS_DATA=pixels)
      6: data_found[ii]=GetProperty('ur_pix',	WDS_DATA=pixels)
      endcase

      if data_found[ii] then begin
        dataset_name = dataset_name + ' ' + state.positions[ii]
        list_control, pix_list, pixels, /APPEND
      endif
    endif ;button_flags[ii]
  endfor

  list_control, pix_list, pixels, /CONCATENATE, /DESTROY

  if (n_elements(pixels) GT 0) then begin
    if state.map_mode then begin
      list_control, x_list,   /CREATE
      list_control, y_list,   /CREATE
  
      z=GetProperty( 'x_position', WDS_DATA=xdata, LABEL=xlabel )
      z=GetProperty( 'y_position', WDS_DATA=ydata, LABEL=ylabel )

      for ii=0, num_buttons-1 do begin
        if (data_found[ii]) then begin
         case ii of
          2: begin
             list_control, x_list, xdata-1, /APPEND
             list_control, y_list, ydata-1, /APPEND
             end
          5: begin
             list_control, x_list, xdata  , /APPEND
             list_control, y_list, ydata-1, /APPEND
             end
          8: begin
             list_control, x_list, xdata+1, /APPEND
             list_control, y_list, ydata-1, /APPEND
             end
          1: begin
             list_control, x_list, xdata-1, /APPEND
             list_control, y_list, ydata  , /APPEND
             end
          4: begin
             list_control, x_list, xdata  , /APPEND
             list_control, y_list, ydata  , /APPEND
             end
          7: begin
             list_control, x_list, xdata+1, /APPEND
             list_control, y_list, ydata  , /APPEND
             end
          0: begin
             list_control, x_list, xdata-1, /APPEND
             list_control, y_list, ydata+1, /APPEND
             end
          3: begin
             list_control, x_list, xdata  , /APPEND
             list_control, y_list, ydata+1, /APPEND
             end
          6: begin
             list_control, x_list, xdata+1, /APPEND
             list_control, y_list, ydata+1, /APPEND
             end
         endcase
        endif
      endfor
        
      list_control, x_list, xdata, /CONCATENATE, /DESTROY
      list_control, y_list, ydata, /CONCATENATE, /DESTROY
      
      if (state.relabel_flag) then begin        
        state.relabel_flag = 0   
        dataset_3d, state.dataset_3d, xdata, ydata, pixels, $
        	    DATASET_NAME=state.dataset_name, /UNITY_ASPECT,$
        	    XTIT=xlabel, YTIT=ylabel, ZTIT='Island Pixel Values (DN)'
        	    
      endif else begin
        dataset_3d, state.dataset_3d, xdata, ydata, pixels, $
        	    DATASET_NAME=state.dataset_name
      endelse
      
    endif else begin
      if (state.relabel_flag) then begin        
        state.relabel_flag = 0   
        dataset_1d, state.dataset_1d, pixels, DATASET_NAME=state.dataset_name,$
		    XTITLE='Island Pixel Values (DN)'
      endif else begin
        dataset_1d, state.dataset_1d, pixels, DATASET_NAME=state.dataset_name
      endelse
    endelse ;dataset_1d
  endif ; data found
endif ;redisplay_flag SET


;; Save the state structure.
parent = state.parent
widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Destroy the widget if desired.
if (destroy_flag) then widget_control, parent, /DESTROY

return,0
end


;========================================================================
FUNCTION eb_island_display, manager_widget, dataset_name, MAP=map, $
			    UVALUE=uvalue, TITLE=title, GROUP=group

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'Event Island Analysis'
if NOT keyword_set(group)  then group  = 0

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.
sz = get_screen_size()

parent = plot_window_topbase(UVALUE=uvalue, TITLE=title, GROUP=group,$
			     XOFFSET=sz[0]-680, YOFFSET=sz[1]-780)

top_base = widget_base( parent, /COLUMN, $
			EVENT_FUNC='EbIslandDisplayEvent' )

 if keyword_set(map) then begin
   dataset_3d, dataset_3d, PARENT_WIDGET=top_base, STAT_CODE=3
   dataset_1d = 0L
 endif else begin
   dataset_1d, dataset_1d, PARENT_WIDGET=top_base
   dataset_3d = 0L
 endelse
 

 base = widget_base(top_base, /ROW, SPACE=1, XPAD=1, YPAD=1 )

 positions = ['Upper-left', 'Left', 'Lower-left', $
	      'Top', 'Center', 'Bottom', $
	      'Upper-right', 'Right', 'Lower-right']

 position_buttons = cw_bgroup( base, positions, COLUMN=3, /NONEXCLUSIVE, $
			       SPACE=1, XPAD=1, YPAD=1, LABEL_LEFT='PIXEL:' )
 widget_control, position_buttons, SET_VALUE=[1,0,1, 0,0,0, 1,0,1]

 replot_button = widget_button(base,VALUE='Replot')

 destroy_button = widget_button( base,VALUE='Dismiss' )



;; Create the widget state structure.

state = { parent:parent, $
	; The id of the Manager Widget.
	manager_widget:manager_widget, map_mode:keyword_set(map), $

	;IDs of widgets that generate events or have to be updated.
	top_base:top_base, $
	dataset_1d:dataset_1d, dataset_3d:dataset_3d, $
	position_buttons:position_buttons, replot_button:replot_button, $
	destroy_button:destroy_button, $

	; Other stuff
	positions:positions, $
	dataset_name: dataset_name, relabel_flag:1 }


;; Realize the widget and register with manager.
widget_control, parent, /REALIZE

;; Simulate an event to initially draw the display.
event = {ID:top_base, TOP:top_base, HANDLER:0L, NAME:'working_set_fresh', $
	 DATASET_NAME:dataset_name}
widget_control, top_base, SEND_EVENT=event

xmanager, 'EbIsland', parent, GROUP_LEADER=group, $
	  EVENT_HANDLER='PlotWindowTopbaseEventFn', /NO_BLOCK
	  
;; Store the state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

return, top_base
end


