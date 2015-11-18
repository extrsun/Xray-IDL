PRO CONTINENT,P1,P2,P3,P4,C
;+
; NAME:
;	CONTINENT
; PURPOSE:
;	generates cylindrical projection of continental outlines in
;	the default viewport using SUPMAP data.  Optional parameter allows
;	latitude coordinates to be plotted as the sin of latitude.
; CALLING SEQUENCE:
;	CONTINENT P1,P2,P3,P4,[,C]
; INPUTS:
;	P(n) arguments determine what part of the world is plotted.
;		P1,P2  are the (latitude,longitude) coordinates of
;			the southwest corner of the map.
;		P3,P4  are the (latitude,longitude) coordinates of
;			the northeast corner of the map.
;	  - coordinates are in degrees.
;	  - longitude is measured +/- east longitude.  Longitude is 
;		constrained to be between -180 and +180 degrees east
;		of Grenwich.
;	  - if P1,P2,P3 and P4 are all zero then
;	    the whole world is plotted.
;	  - If !NOERAS is non-zero, the previous plot is overplotted with
;	    the desired continental outlines.  In addition, if !NOERAS is
;	    negative, the border around the map is suppressed.
;	Example:
;		To make a cylindrical plot of the region bounded
;		on the west by longitude -90, on the north by latitude
;		60, on the east by longitude 10 and on the south by
;		latitude 40 the following sequence should be followed:
;			CONTINENT,40,-90,60,10
;		The continent outlines will be drawn for the region.
; OPTIONAL INPUT:
;	C, = 0 or not supplied will cause latitude to be plotted as a 
;		linear scale.
;	   = 1, causes latitude to be plotted as the sin(lat).
; OUTPUTS:
;	produces the continental outlines in the current VIEWPORT.
;	See SET_VIEWPORT for details on how to manipulate the location
;	of the map.
; SIDE EFFECTS:
;	Same as PLOTS.	Edges sometimes do not connect to border of map.
;	Developed primarily for Tek. 4014 type terminals.  May be slow
;	on other types of image display terminals.
; RESTRICTIONS:
;	As noted in the NCAR ducumentation, the SUPMAP data exhibits 
;	approximatly 1 degree precision.
; PROCEDURE:
;	Data for the continent outlines is contained in a file called:
;			CONTINENT.DAT
;	Its directory location should be assigned to the logical name:
;			ZAUX:
;	The data file CONTINENT.DAT is an unformatted copy of continent
;	data contained in the NCAR SUPMAPDAT.DAT file.  Procedure for plotting
;	is straight forward.  It uses PLOTS which is device independent.
; MODIFICATION HISTORY:
;	August, 1985		written, Leonard Kramer (Univ. Resrch. Found./
;			      	  U. of Maryland. {URF})  (301) 286-4769
;	February, 1986		modified to allow the !NOERAS system variable
;				to control the plotting.
;	March, 1986		modified to allow sin(latitude) plotting.
;	August, 1990		modified for new IDL, JDN @ ACC
;-
	ON_ERROR,1
	
	XN=!X.range(0) & XX=!X.range(1) & YN=!Y.range(0) & YX=!Y.range(1)
	
	NPAR=N_PARAMS(0)
	IF(NPAR LT 4) THEN BEGIN
	  PRINT, 'not enough parameters'
	  RETURN
	ENDIF
	!X.range(0)=P2 & !X.range(1)=P4 & !Y.range(0)=P1 & !Y.range(1)=P3
	IF (N_ELEMENTS(C) EQ 0) THEN C=0		; March, 1986
	IF NPAR EQ 0 THEN P=0 ; cylindrical projection
	GET_LUN,LUN
	if !version.os eq 'vms' then $
		OPENR,LUN,getlog('ZAUX')+'continent.dat/unformatted' $
	else $
		OPENR,LUN,getlog('ZAUX')+'continent.dat'
	NPTS=0   ; a 2 byte integer
	MINMAX=FLTARR(4)
	; do a cylindrical projection.
	IF(!X.range(0) GT 180.) THEN !X.range(0)=!X.range(0)-360.
	IF(!X.range(1) GT 180.) THEN !X.range(1)=!X.range(1)-360.
	IF ((!X.range(0) EQ 0) AND (!X.range(1) EQ 0) $   ; set default values.
	      AND (!Y.range(0) EQ 0) AND (!Y.range(1) EQ 0)) THEN BEGIN
	  !X.range(0)=0.0 & !X.range(1)=0.0 
 	  !Y.range(0)=-90. & !Y.range(1)=90.
	ENDIF
	IF (C) THEN BEGIN 			;march '86
	  RADIAN=3.14159/180.0
	  !Y.range(0)=SIN(RADIAN*!Y.range(0)) 
          !Y.range(1)=SIN(RADIAN*!Y.range(1)) 
	ENDIF
	IF (!P.NOERASE EQ 0) THEN ERASE  ; erase previous plot 
	IF (!X.range(0) LT !X.range(1)) THEN BEGIN  ; straight forward plotting.
	  SET_XY,!X.range(0),!X.range(1),!Y.range(0),!Y.range(1)
	  LINETYPE=!P.LINESTYLE  & !P.LINESTYLE=0
	  IF(!P.NOERASE GE 0) THEN BEGIN
	    PLOTS,[!X.range(0),!X.range(0),!X.range(1),!X.range(1),$
                   !X.range(0)],[!Y.range(0),!Y.range(1),!Y.range(1),$
                   !Y.range(0),!Y.range(0)] 
	  ENDIF
	  !P.LINESTYLE=LINETYPE
	  WHILE NOT EOF(LUN) DO BEGIN
	    FORRD,LUN,NPTS,MINMAX & NPTS=NPTS/2
	    LON=FLTARR(NPTS) & LAT=FLTARR(NPTS) & FORRD,LUN,LAT,LON
	    IF(C) THEN LAT=SIN(RADIAN*LAT)
	    T=WHERE((LON GE !X.range(0)) AND (LON LE !X.range(1)) AND $
		 (LAT GE !Y.range(0)) AND (LAT LE !Y.range(1)))
	    SZ=SIZE(T)
	    IF(SZ(0) EQ 0) THEN T=0
	    PLOTS,LON(T),LAT(T)
	  ENDWHILE
	ENDIF ELSE BEGIN  ; !X.range(0) must be greater than or equal to 
                          ; !X.range(1)
	  XMAX=!X.range(1) & !X.range(1)=!X.range(1)+360.
	  SET_XY,!X.range(0),!X.range(1),!Y.range(0),!Y.range(1)
	  LINETYPE=!P.LINESTYLE  & !P.LINESTYLE=0
	  IF(!P.NOERASE GE 0) THEN BEGIN
	    PLOTS,[!X.range(0),!X.range(0),!X.range(1),!X.range(1),$
                   !X.range(0)],[!Y.range(0),!Y.range(1),!Y.range(1),$
                   !Y.range(0),!Y.range(0)] 
	  ENDIF
	  WHILE NOT EOF(LUN) DO BEGIN
            !P.LINESTYLE=LINETYPE
	    FORRD,LUN,NPTS,MINMAX & NPTS=NPTS/2
	    LON=FLTARR(NPTS) & LAT=FLTARR(NPTS) & FORRD,LUN,LAT,LON
	    IF(C)THEN LAT=SIN(RADIAN*LAT)
	    S=WHERE(LON LE XMAX)
	    SZ=SIZE(S)
	    IF(SZ(0) EQ 0) THEN S=0
	    LONW=LON(S)+360 & LATW=LAT(S)
	    T=WHERE(LON GE XMAX)
	    SZ=SIZE(T)
	    IF(SZ(0) EQ 0) THEN T=0
	    LONE=LON(T)     & LATE=LAT(T) 
	    U=WHERE((LONW GE !X.range(0)) AND (LONW LE !X.range(1)) AND $
		     (LATW GE !Y.range(0)) AND (LATW LE !Y.range(1)))
	    SZ=SIZE(U)
	    IF(SZ(0) EQ 0) THEN U=0
	    PLOTS,LONW(U),LATW(U)	   
	    U=WHERE((LONE GE !X.range(0)) AND (LONE LE !X.range(1)) AND $
		     (LATE GE !Y.range(0)) AND (LATE LE !Y.range(1)))
	    SZ=SIZE(U)
	    IF(SZ(0) EQ 0) THEN U=0
	    PLOTS,LONE(U),LATE(U)	   
	  ENDWHILE
	  !X.range(1)=XMAX
  	ENDELSE
	FREE_LUN,LUN
	!X.range(0)=XN & !X.range(1)=XX & !Y.range(0)=YN & !Y.range(1)=YX
	RETURN
END
