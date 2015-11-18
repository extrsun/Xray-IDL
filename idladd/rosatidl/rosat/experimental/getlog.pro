Function GETLOG,lname
;+
; NAME:
;	GETLOG
; PURPOSE:
;	Returns the correct logical directory for the given operating system.
;	E.G. dat: for vms, $DAT/ for unix.
; CALLING SEQUENCE:
;	GETLOG,lname
; INPUTS:
;	lname	- the base name of the logical (without special characters).
;
; OUTPUTS:
;	Returns appropriate string.
;
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	Assumes that the created directory logical will have meaning to the host
;	operating system.
; PROCEDURE:
;	The operating system in !version.os is checked. If it equals:
;
;		'vms'		then a ':' is appended.
;
;		else		unix os is assumed and the input string is
;				uppercased, a '$' is prepended and a '/' is
;				appended.
;
; MODIFICATION HISTORY:
;	Written, JDNeill, May, 1990.
;	Modified, JDNeill,Sep, 1990 -- for unix return full path instead of
;		just environment variable name.
;-
;-----------------------------------------------------------------------------
	if !version.os eq 'vms' then $
		return,lname+':' $
	else $	; assume unix os
		return,getenv(strupcase(lname))+'/'
;
	end	; getlog.pro
