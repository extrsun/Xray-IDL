;+
;========================================================================
;;;
;;; FILE NAME:    $Id: diffraction_display.pro 1057 2000-04-10 10:12:41Z patb $
;;;
;;; DESCRIPTION:  Diffraction Pattern Display widget.  This widget displays
;;;               the number of photons vs. the distance from an arbitrary
;;;               point.
;;;
;;;               The manager_widget parameter is the widget ID of the 
;;;               Manager Widget (the widget that is calling this function).
;;;
;;;               UVALUE, TITLE, and GROUP are the normal IDL widget keywords.
;;;               The UVALUE of the first child of the base is used to store
;;;               a structure representing the state of the display widget.  
;;;
;;;               The HANDLE keyword specifies the parent handle to be used
;;;               for freeing all Event Browser handles upon exit.  If a
;;;               parent handle is not specified, then the Diffraction
;;;               Display Widget frees its (and its children's) handles
;;;               when the Dismiss button is pressed.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1997, Pennsylvania State University
;;;
;;; NOTES:        See the document "X-ray Event Browser Software Design
;;;               Specification"
;;;
;-
;========================================================================


;==========================================================================
FUNCTION GetDiffractionData, rp

; Get the event position information
z=GetProperty('x_position', WDS_DATA=x)
z=GetProperty('y_position', WDS_DATA=y)

print, 'Using reference point', rp(0), rp(1)

; Calculate the distance from each event to the reference point
diffraction_data = sqrt( ((x-rp(0))^2.0) + ((y-rp(1))^2.0) )

return, diffraction_data
end

;==========================================================================
FUNCTION DiffractionPatternDisplayEvent, event

destroy_flag   = 0

;; Get the widget state structure.
top_base = event.handler
widget_control, top_base,  GET_UVALUE=state, /NO_COPY, $
		/HOURGLASS


;; Handle the event; branch first based on which widget generated it.
case event.id of

  ;------------------------------------------------------------------------
  state.dataset_1d: 
    ; Do nothing.  We don't propogate filtering done in dataset_1d.

  ;------------------------------------------------------------------------
  state.top_base: $
    begin
    case event.name of
      'working_set_stale': $
	begin
	widget_control, top_base, SENSITIVE=0
	end

      'working_set_delete': 

      'working_set_fresh' : $
	begin
	widget_control, top_base, SENSITIVE=1

	state.dataset_name = event.dataset_name

        ;; Update the dataset_1d widget.

        diffraction_data=GetDiffractionData(state.ref_point)
        dataset_1d, state.dataset_1d, diffraction_data, $
			  DATASET_NAME=state.dataset_name, $
			  DESCRIPTION=state.dataset_name, $
		          XTITLE='Distance from Reference Point'
	end

      else: message, 'event not handled'
    endcase
    end

  ;------------------------------------------------------------------------
  state.ref_point_button: $
    begin
    ; Prompt the user for a fiducial point for calculating distances.
    desc = [ '0, FLOAT, 0.0, LABEL_TOP=x, WIDTH=10', $
             '0, FLOAT, 0.0, LABEL_TOP=y, WIDTH=10', $
             '2, BUTTON, OK, QUIT' ]
    a = cw_form(desc, TITLE='Enter a Reference Point')
    ref_point    = fltarr(2)
    ref_point(0) = a(0).TAG0
    ref_point(1) = a(0).TAG1
    state.ref_point = ref_point
    diffraction_data = GetDiffractionData( ref_point )
    dataset_1d, state.dataset_1d, diffraction_data, $
			  DATASET_NAME=state.dataset_name, $
			  DESCRIPTION=state.dataset_name, $
		          XTITLE='Distance from Reference Point'

    ; Update the label widget which shows the reference point
    str = 'Reference Point (' + string(ref_point(0), FORMAT='(F7.2)') + $
			    ',' + string(ref_point(1), FORMAT='(F7.2)') + ')'
    widget_control, state.ref_point_label, SET_VALUE=str
    end

  ;------------------------------------------------------------------------
  state.destroy_button: $
    begin
    destroy_flag   = 1
    end

  ;------------------------------------------------------------------------
  else: message, 'event not handled'
endcase

;; Free all child handles
if (destroy_flag) then handle_free, state.dpd_handle


;; Save the state structure.
parent = state.parent
widget_control, top_base, SET_UVALUE=state, /NO_COPY


;; Destroy the widget if desired.
if (destroy_flag) then widget_control, parent, /DESTROY

return,0
end

;========================================================================
FUNCTION diffraction_display, manager_widget, dataset_name, $
	 UVALUE = uvalue, TITLE=title, GROUP=group, HANDLE=p_handle

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'Diffraction Pattern'
if NOT keyword_set(group)  then group  = 0

if (NOT keyword_set(p_handle)) then begin
  dpd_handle = handle_create()
endif else dpd_handle = handle_create(p_handle)
 

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

parent = plot_window_topbase(UVALUE=uvalue, TITLE=title, GROUP=group,$
			     XOFFSET=440, YOFFSET=250)

top_base = widget_base( parent, /COLUMN, $
			EVENT_FUNC='DiffractionPatternDisplayEvent' )

 dataset_1d, dataset_1d, PARENT_WIDGET=top_base

 ; Prompt the user for a fiducial point for calculating distances.
 desc = [ '0, FLOAT, 0.0, LABEL_TOP=x, WIDTH=10', $
          '0, FLOAT, 0.0, LABEL_TOP=y, WIDTH=10', $
          '2, BUTTON, OK, QUIT' ]
 a = cw_form(desc, TITLE='Enter a Reference Point')
 ref_point    = fltarr(2)
 ref_point(0) = a(0).TAG0
 ref_point(1) = a(0).TAG1

 button_base = widget_base(top_base, /ROW, /ALIGN_RIGHT )
   rp_base = widget_base (button_base, /FRAME, /ROW)
     str = 'Reference Point (' + string(ref_point(0), FORMAT='(F7.2)') + $
                             ',' + string(ref_point(1), FORMAT='(F7.2)') + ')'
     ref_point_label  = widget_label ( rp_base, VALUE=str )
     ref_point_button = widget_button( rp_base, $
                                       VALUE='Change Reference Point' )
   destroy_button = widget_button( button_base,VALUE='Dismiss Display' )


;; Create the widget state structure.

state = { parent:parent, $
	; The id of the Manager Widget.
	manager_widget:manager_widget, $

	;IDs of widgets that generate events or have to be updated.
	top_base:top_base, dataset_1d:dataset_1d, $
	ref_point_button:ref_point_button, $
        ref_point_label:ref_point_label, destroy_button:destroy_button, $

	; Other stuff
        dpd_handle:dpd_handle, $
	dataset_name: dataset_name, $
        ref_point:ref_point }


;; Realize the widget and register with the manager.
widget_control, parent, /REALIZE

xmanager, 'diffraction pattern display', parent, GROUP_LEADER=group, $
	  EVENT_HANDLER='PlotWindowTopbaseEventFn', /NO_BLOCK
	  
diffraction_data=GetDiffractionData(state.ref_point)
dataset_1d, state.dataset_1d, diffraction_data, $
			  DATASET_NAME=state.dataset_name, $
			  DESCRIPTION=state.dataset_name, $
		          XTITLE='Distance from Reference Point'


;; Store the state structure.
widget_control, top_base, SET_UVALUE=state, /NO_COPY

return, top_base
end


