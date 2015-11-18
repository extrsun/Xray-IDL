pro stringad_2,ra,dec,rac,decc
;+ 
; NAME:
;	STRINGAD_2
; PURPOSE:
;	Converts a string of sexigesimal coordinates separated with ':'
;	to their decimal form.
;
; CALLING SEQUENCE:
;	STRINGAD_2, RA, DEC,rac,decc
; INPUT:
;	COORDS    A string of coordinates (e.g. '17:00:45.2', '25:4:32.4')
;		It should have six numbers delimited by spaces
; OUTPUT:
;	RA        Right Ascension, decimal degrees, scalar
;	DEC       Declination, decimal degrees, scalar
; PROCEDURES CALLED:
;	Gettok 
; HISTORY:
;	  written by wqd, 10/5/95
;-
;  On_error,2
  if ( N_params() LT 1 ) then begin
    print,'Call: IDL> stringad_2,ra,dec,rac,decc'
    print,"e.g.: IDL> STRINGAD,'17:00:45.2', '25:4:32.4',ra,dec"
    return
  endif
ras=ra
decs=dec
ih=fix(gettok(ras,':'))
im=fix(gettok(ras,':'))
is=float(ras)
jd=fix(gettok(decs,':'))
jm=fix(gettok(decs,':'))
js=float(decs)
trans_radian,ih,im,is,jd,jm,js,rac,decc,/deg
  return
  end
