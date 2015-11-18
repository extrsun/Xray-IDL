;+
;========================================================================
;;;
;;; FILE NAME:    $Id: nonmodalmessage.pro 709 1998-11-13 13:35:28Z patb $   
;;;
;;; DESCRIPTION:  Non-modal Message Widget
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
PRO NomMsgEvent, event
widget_control, event.handler, /DESTROY, BAD_ID=bad_id
return
end

FUNCTION NonmodalMessage, message, GROUP_LEADER=group, UVALUE=uvalue, $
			  TITLE=title, XOFFSET=xoffset, YOFFSET=yoffset, $
			  XSIZE=xsize, YSIZE=ysize, POSITIONING_PARENT=pp, $
			  TEXT_WIDGET=msgtxt

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

base =    widget_base(TITLE=title, UVALUE=uvalue, $
		      GROUP_LEADER=group, /COLUMN,  $
		      XOFFSET=xoffset, YOFFSET=yoffset)

;msgtxt =  WIDGET_TEXT(base, VAL = message, EDITABLE=0, /SCROLL, $
;		      YSIZE=20<n_elements(message))

msgtxt =  WIDGET_TEXT(base, VAL=message, EDITABLE=0, XSIZ=xsize, YSIZ=ysize)

quit = widget_button(base, VALUE='Dismiss')

WIDGET_CONTROL, base, /REALIZE

xmanager, 'NonmodalMessage', base, GROUP_LEADER=group, /NO_BLOCK, $
	    EVENT_HANDLER='NomMsgEvent'


return, base

END
