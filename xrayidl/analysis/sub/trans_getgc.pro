;.......................................................................
;
pro trans_getgc,ra,dec,DDL,DDLA
;
;-------------------------------------------------------------------------
;   The subroutine GETGC transfers the earth coordinates right accension
; and declination to the galatic coordinates logitude and latitude
;------------------------------------------------------------------------
if(n_params() eq 0) then begin
   print,'CALL SEQUENCE - trans_getgc,ra,dec,DDL,DDLA'
   return
endif
;......................................................................
;
	C=COS(62.6*!DPI/180.)
	S=SIN(62.6*!DPI/180.)
	R=282.25*!DPI/180.
	SLAT=SIN(DEC)*C-COS(DEC)*SIN(ra-R)*S
	SLAT=SLAT < 1.0
	SLAT=((SLAT+1.) > 0.0 ) -1.
;
	RLAT=ASIN(SLAT)
	DDLA=RLAT*180.0d/!DPI
	EC=COS(DEC)*COS(ra-R)/COS(RLAT)
	ES=(COS(DEC)*SIN(ra-R)*C+SIN(DEC)*S)/COS(RLAT)
	ES = ES < 1.0
	ES =((ES+1.) > 0.0) -1. 
	EC = EC < 1.0
	EC =((EC+1.) > 0.0) -1. 
;
	DDL=acos(ec)*180./!dpi
	c=where (es GE 0.,count) 
	if count NE 0 then ddl(c)=ddl(c)+33.
	c=where (es LT 0.,count)
	if count NE 0 then DDL(c)=360.-DDL(c)+33.
	ddl = ddl mod 360.
;
	return
	end	
