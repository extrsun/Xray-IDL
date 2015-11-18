;==========================================================================
;;; $Id: tara_clipboard.pro 3441 2009-06-17 00:18:01Z patb $
;;; Clipboard utility for TARA tools.
;;; Patrick Broos, Aug 2000
;==========================================================================

PRO TaraClipboardEvent, event

COMMON TARA_CLIPBOARD, base, msgtxt, save_button, file_button, flabel, fname

case Event.ID of
 ;; Append the clipboard contents to the file.
 save_button: $
  begin
  widget_control, msgtxt, GET_VALUE=text
  openu, unit, fname, /APPEND, /GET_LUN
  printf, unit, text, F='(A)'
  free_lun, unit

  msg = string(numlines(fname),fname,f='(I0," lines in ",A)')
  widget_control, flabel, SET_VALUE=msg
  end

 file_button: $
  begin
  fdecomp, fname, disk, item_path, item_name, item_qual
  new_file = dialog_pickfile(GROUP=base, PATH=item_path)
  if (new_file NE '') then begin
    fname=new_file
    widget_control, flabel, SET_VALUE=fname
  endif
  end

 msgtxt:
endcase
return
end


PRO tara_clipboard, filename, POST=post

COMMON TARA_CLIPBOARD

;; Handle undefined vars in first call.
if (n_elements(fname) EQ 0) then fname = 'tara_clipboard.txt'

;; We should create the widget if there has never been one, or if
;; tara_clipboard is called (by user) without POST.
create_widget = 0
if (n_elements(base) EQ 0) then begin
  base = -1L
  create_widget = 1
endif

if ((NOT keyword_set(post)) AND (widget_info(base,/VALID) EQ 0)) then $
  create_widget = 1


; Create the widget if necessary.
if (create_widget) then begin
  device,get_screen_size=screen_size
  xoffset= screen_size(0)/2 - 200
  yoffset= screen_size(1)   - 120
  base =    widget_base(TITLE='TARA Clipboard', /COLUMN,  $
		      XOFFSET=xoffset, YOFFSET=yoffset)

  msgtxt =  WIDGET_TEXT(base, EDITABLE=1, XSIZ=80, YSIZ=2)

  cbase = widget_base(base, /ROW )
   save_button = widget_button(cbase, VALUE='Save')
   file_button = widget_button(cbase, VALUE='File')
   flabel = widget_label( cbase, /DYNAMIC_RESIZE, VALUE=fname )

  WIDGET_CONTROL, base, /REALIZE

  xmanager, 'tara_clipboard', base, /NO_BLOCK, /JUST_REG,$
	    EVENT_HANDLER='TaraClipboardEvent'
endif

if (n_elements(filename) NE 0) then begin
  fname=filename
  widget_control, flabel, SET_VALUE=fname
endif

;; Display POST message in widget (if it exists) or by printing.
if keyword_set(post) then begin
  text = ['---------------------------------------------------------------------',post,'']  
  if (widget_info(base,/VALID) EQ 1) then begin
    widget_control, msgtxt, SET_VALUE=text
  endif else print, text
endif
return
end
