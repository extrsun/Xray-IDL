;+
;========================================================================
;;;
;;; FILE NAME:    $Id: coordsystem_manager.pro 311 1997-07-15 10:13:57Z patb $
;;;
;;; DESCRIPTION:  Coordinate System Manager
;;;
;;;               The file contains routines for saving and restoring the
;;;               system variables which define plot coordinate systems in
;;;               draw widgets.  These system variables are stored in a
;;;               structure attached to the UVALUE property of the draw
;;;               widget.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        
;;;               SAVE OPTION: Saves the coordinate system for the window
;;;               associated with the draw widget.
;;;
;;;               RESTORE OPTION:  Sets the "current window" to be the one
;;;               associated with the draw widget.  If a coordinate system
;;;               has previously been saved for that window, it is restored
;;;               by writing to the system variables.  The draw widget is
;;;               assumed to be already realized!
;;;
;-
;==========================================================================
PRO coordsystem_manager, draw_widget, RESTORE=restore, SAVE=save

if (keyword_set(restore)) then begin
  widget_control, draw_widget, GET_VALUE=window_number, GET_UVALUE=struct
  wset, window_number

  if (0 NE n_elements(struct)) then begin
    !P = struct.p_sysvar
    !X = struct.x_sysvar
    !Y = struct.y_sysvar
    !Z = struct.z_sysvar
  endif
endif

if (keyword_set(save)) then begin
  struct = {p_sysvar: !P, x_sysvar: !X, y_sysvar: !Y, z_sysvar: !Z}

  widget_control, draw_widget, SET_UVALUE=struct
endif

return
end

