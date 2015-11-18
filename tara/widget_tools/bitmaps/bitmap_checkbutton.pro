;+
;========================================================================
;;;
;;; FILE NAME:    $Id: bitmap_checkbutton.pro 284 1997-04-14 16:11:34Z patb $
;;;
;;; DESCRIPTION:  Bitmap Checkbutton Compound Widget
;;;
;;;               This compound widget implements a checkbutton with a
;;;               bitmap button label and an adjacent text label.  A file
;;;               named (bitmap_name + '.bm') must exist in the IDL path.
;;;
;;;               This widget has a VALUE property that is a structure
;;;               containing the slots listed below.  
;;;
;;;               checked_flag:  TRUE if the button is currently checked
;;;               label_text:    string to appear next to button
;;;
;;;               This widget generates an event for its parent whenever the
;;;               user checks or unchecks the button.  The parent should
;;;               then GET this widget's VALUE to obtain the button status.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;-
;==========================================================================
;;; Widget SET_VALUE Routine
;==========================================================================
PRO SetBitmapCheckbuttonValue, top_base, struct

;; Get the state structure.
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

;; Store the new values.
state.checked_flag = struct.checked_flag
state.label_text   = struct.label_text

widget_control, state.button, SET_BUTTON=struct.checked_flag
widget_control, state.label,  SET_VALUE =struct.label_text

;; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return
end

;==========================================================================
;;; Widget GET_VALUE Routine
;==========================================================================
FUNCTION GetBitmapCheckbuttonValue, top_base

;; Get the state structure.
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY

;; Create the return structure.
struct = { checked_flag:state.checked_flag, label_text:state.label_text }

;; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return, struct
end


;==========================================================================
;;; Widget Event Handler Function
;==========================================================================
FUNCTION BitmapCheckbuttonEvent, event

;; Get the state structure.
top_base = event.handler
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY


;; Process the event.
case event.ID of
  ;------------------------------------------------------------------------
  state.button: $
    begin
    state.checked_flag = event.select
    end

  ;------------------------------------------------------------------------
  else: message, 'event not handled'
endcase

;; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return, { ID:top_base, TOP:event.top, HANDLER:0L }
end

;==========================================================================
;;; Widget Creation Routine

FUNCTION bitmap_checkbutton, parent, bitmap_name, label_text, checked_flag, $
				  UVALUE=uvalue

if NOT keyword_set(uvalue)      then uvalue = 0

;; Read bitmap file.
bitmap = bytarr(2,16)
filename = find_with_def( bitmap_name + '.bm', !PATH )
openr, unit, filename, /GET_LUN
readu, unit, bitmap
free_lun, unit

;; Set the asca grade based on the grade code.  This information is passed
;; into bitmap_checkbutton() via the user value mechanism.
case uvalue.code of
	'00'XB: asca_name = '  '
	'40'XB: asca_name = '  '
	'02'XB: asca_name = '  '
	'08'XB: asca_name = '  '
	'10'XB: asca_name = '  '
	'0A'XB: asca_name = '  '
	'12'XB: asca_name = '  '
	'48'XB: asca_name = '  '
	'50'XB: asca_name = '  '
	'0B'XB: asca_name = '  '
	'16'XB: asca_name = '  '
	'68'XB: asca_name = '  '
	'D0'XB: asca_name = '  '
	'FF'XB: asca_name = 'other'
endcase

top_base = widget_base( parent, UVALUE=uvalue, /COLUMN, /BASE_ALIGN_CENTER, $
			  FRAME=2, SPACE=1, XPAD=1, YPAD=1, $
			  EVENT_FUNC    ='BitmapCheckbuttonEvent', $
			  FUNC_GET_VALUE='GetBitmapCheckbuttonValue', $
			  PRO_SET_VALUE ='SetBitmapCheckbuttonValue' ) 

;asca_label = widget_label(top_base, VALUE=asca_name, /DYNAMIC_RESIZE )
button_base = widget_base( top_base, /NONEXCLUSIVE, SPACE=1, XPAD=1, YPAD=1 )

button = widget_button( button_base, VALUE=bitmap )
widget_control, button, SET_BUTTON=1

label  = widget_label( top_base, VALUE=label_text, /DYNAMIC_RESIZE )
;label  = widget_label( top_base, VALUE=label_text, XSIZE=100 )


;; Setup state structure.
state = { $
;	asca_label:asca_label, $
	button:button, label:label, $
	
	checked_flag:checked_flag, label_text:label_text }


;; Save state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

return, top_base
END

