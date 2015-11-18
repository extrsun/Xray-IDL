;+
;========================================================================
;;;
;;; FILE NAME:    $Id: timedmessage.pro 4231 2012-05-08 16:17:53Z psb6 $
;;;
;;; DESCRIPTION:  Timed Message Widget
;;;
;;;               Used to display a message in a non-modal dialog box.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        Use the POSITIONING_PARENT keyword to supply the widget ID
;;;               of the caller.  When this is done, the non-modal message
;;;               widget will be displayed on top of the caller with a slight
;;;               offset.
;;;
;-
;==========================================================================
PRO TimedMsgEvent, event


;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st

if (event.ID EQ top_base) || (event.ID EQ st.quit_id) then widget_control, event.handler, /DESTROY, BAD_ID=bad_id
return
end

PRO TimedMessage, base, message, GROUP_LEADER=group, UVALUE=uvalue, $
			  TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
			  XSIZE=xsize, YSIZE=ysize, POSITIONING_PARENT=pp, $
			  LIFETIME=lifetime, NO_QUIT=no_quit, QUIT_LABEL=quit_label, QUIT_ID=quit_id, BUTTON_LABEL=button_label, BUTTON_ID=button_id

if NOT keyword_set(uvalue) then uvalue = 0
if NOT keyword_set(group)  then group = 0
if NOT keyword_set(title)  then title=' '
if (n_elements(xoffset) EQ 0) then begin
  center = ScreenCenter()
  xoffset= center(0) - 150
endif
if (n_elements(yoffset) EQ 0) then begin
  yoffset= 0
endif

if NOT keyword_set(xsize) then begin
  xsize = max( [strlen(message)+1, strlen(title)+3] )
endif

if NOT keyword_set(ysize) then begin
  ysize = n_elements(message) 
endif

; The positioning parent keywords overrides any x or y offsets.
if keyword_set(pp) then begin
  this_widget=pp
  parent = 1
  xoffset = 0
  yoffset = 0
  while (this_widget NE 0) do begin
    geometry=widget_info(this_widget, /GEOMETRY)
    xoffset = xoffset + geometry.xoffset
    yoffset = yoffset + geometry.yoffset
    parent=widget_info(this_widget, /PARENT)
    this_widget=parent
  endwhile
endif

; Create the widget if necessary.
if (n_elements(base) EQ 0) then base = -1L
if (widget_info(base, /VALID_ID) EQ 0) then begin
  base =    widget_base(TITLE=title, UVALUE=uvalue, $
		      GROUP_LEADER=group, /COLUMN,  $
		      XOFFSET=xoffset, YOFFSET=yoffset)

  ;msgtxt =  WIDGET_TEXT(base, VAL = message, EDITABLE=0, /SCROLL, $
  ;		      YSIZE=20<n_elements(message))

  msgtxt =  WIDGET_TEXT(base, EDITABLE=0, XSIZ=xsize, YSIZ=2)

  button_id = keyword_set(button_label) ? widget_button(base, VALUE=button_label) : 0L
    
  quit_id = keyword_set(no_quit) ? 0L : widget_button(base, VALUE=keyword_set(quit_label) ? quit_label : 'Dismiss')

  ;; Save state structure.
  state = {button_id:button_id, quit_id:quit_id}
  widget_control, base, SET_UVALUE=state

  WIDGET_CONTROL, base, /REALIZE

  xmanager, 'TimedMessage', base, GROUP_LEADER=group, /NO_BLOCK, /JUST_REG,$
	    EVENT_HANDLER='TimedMsgEvent'
endif

; Update the text widget.
msgtxt = widget_info( base, /CHILD )
msgtxt_geom = widget_info( msgtxt, /GEOMETRY )
widget_control, msgtxt, XSIZ=xsize > msgtxt_geom.xsize, YSIZ=ysize, SET_VALUE=message

; Schedule a timer event to destroy widget if desired.
if keyword_set(lifetime) then $
  widget_control, base, /CLEAR_EVENTS, TIMER=lifetime

return

END
