;+
;==========================================================================
;;;
;;; FILE NAME:    $Id: grade_distribution.pro 872 1999-09-10 15:33:26Z patb $
;;;
;;; DESCRIPTION:  Grade Distribution Display Widget for X-ray Event Browser
;;;               application.
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
;;;               The UVALUE of the first child of the base is used to store
;;;               a structure representing the state of the display widget.  
;;;
;-
;==========================================================================
;==========================================================================
PRO GradeDistDisplayEvent, event

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
      'working_set_stale': $
	begin
	widget_control, top_base, SENSITIVE=0
	end

      'working_set_delete': 

      'working_set_fresh' : $
	begin
	state.dataset_name = event.dataset_name
	widget_control, top_base, SENSITIVE=1
	redisplay_flag = 1
	end

      else: message, 'event not handled'
    endcase
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
  ; Map ACIS grades to ASCA grades.
  z=GetProperty('asca_grade', WDS_DATA=asca_grades)

  ; Compute percentages.
  h = histogram( asca_grades, MIN=0, MAX=7 )
  text = strarr(8)
  total_num = float( n_elements( asca_grades ) )
  fmt = '( "g",I0,", ",I6," events =",F6.2," +-",F6.2," %" )'
  for ii = 0,7 do begin
    text(ii) = string( ii, h(ii), 100*h(ii)/total_num, $
    				  100*sqrt(h(ii))/total_num, f=fmt ) 
  endfor

  widget_control, state.text, SET_VALUE=text
endif


;; Save the state structure.
widget_control, top_base,  SET_UVALUE=state, /NO_COPY


;; Destroy the widget if desired.
if (destroy_flag) then widget_control, top_base, /DESTROY

return
end

;==========================================================================
FUNCTION grade_distribution, manager_widget, dataset_name, $
	 UVALUE = uvalue, TITLE=title, GROUP=group

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(title)  then title  = 'Grade Distribution'
if NOT keyword_set(group)  then group  = 0


;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

top_base = widget_base( UVALUE=uvalue, TITLE=title, GROUP=group, /COLUMN, $
			XOFFSET=440, YOFFSET=150, /BASE_ALIGN_RIGHT)

 label = widget_label( top_base, VALUE='ASCA Grades', /ALIGN_CENTER )

 text = widget_text( top_base, XSIZE=40, YSIZE=8 )

 destroy_button = widget_button(top_base,VALUE='Dismiss Display',/ALIGN_RIGHT)


;; Create the widget state structure.

state = { $
	; The id of the Manager Widget.
	manager_widget:manager_widget, $

	;IDs of widgets that generate events or have to be updated.
	top_base:top_base, text:text, destroy_button:destroy_button, $

	; Other stuff
	dataset_name: dataset_name }


;; Realize the widget.
widget_control, top_base, /REALIZE

;; Simulated events generated my the manager widget.
event = {ID:top_base, TOP:top_base, HANDLER:0L, NAME:'working_set_fresh', $
	 DATASET_NAME:dataset_name}
widget_control, top_base, SEND_EVENT=event

;; Register with manager.
xmanager, 'grade dist display', top_base, GROUP_LEADER=group, $
	  EVENT_HANDLER='GradeDistDisplayEvent', /NO_BLOCK
	  
;; Store the state structure.
widget_control, top_base,  SET_UVALUE=state, /NO_COPY

return, top_base
end



