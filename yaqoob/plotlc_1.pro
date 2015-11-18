PRO plotlc_1, flist, num=num,NOHAT=hat, HATLENGTH=hln, ERRTHICK=eth, $
        ERRSTYLE=est, TYPE=itype, BACKGROUND=back, CHANNEL=chan, CHARSIZE=chsiz, $
        CHARTHICK=chthck, COLOR=color, DATA=data, DEVICE=device, $
	FONT=font, LINESTYLE=linest, NOCLIP=noclip, NODATA=nodata, $
	NOERASE=noerase, NORMAL=normal, NSUM=nsum, PSYM=psym, $
	SUBTITLE=subtit, SYMSIZ=symsiz, T3D=t3d, THICK=thick, TICKLEN=ticklen, $
	TITLE=title, XCHARSIZE=xchsiz, XMARGIN=xmargn, XMINOR=xminor, $
	XRANGE=xrange, XSTYLE=xstyle, XTICKLEN=xtickln, XTICKNAME=xticknm, $
	XTICKS=xticks,XTICKV=xtickv,XTITLE=xtitle,XTYPE=xtype,XTICK_GET=xtget, $
	YCHARSIZE=ychsiz, YMARGIN=ymargn, YMINOR=yminor, $
	YRANGE=yrange, YSTYLE=ystyle, YTICKLEN=ytickln, YTICKNAME=yticknm, $
	YTICKS=yticks,YTICKV=ytickv,YTITLE=ytitle,YTYPE=ytype,YTICK_GET=ytget
;+
; NAME:
;	PLOTLC_1
; PURPOSE:
;	Plot one or more light curves with accompanying X or Y error bars.
; CALLING SEQUENCE:
;	plotlc_1,flist,num=num [, TYPE = type] [,/NOHAT, HATLENGTH=hln,
;		 ERRTHICK=eth, ERRSTYLE=est]
; INPUTS:
;	flist => structure with light curve points and error bars
;        num  => no of data set plotted on same plot
;       Optional keyword parameters
;	TYPE = type of plot produced.  The possible types are:
;		ITYPE = 0 :	X Linear - Y Linear  (default)
;		ITYPE = 1 :	X Linear - Y Log
;		ITYPE = 2 :	X Log    - Y Linear
;		ITYPE = 3 :	X Log    - Y Log
;	       Actually, if 0 is specified, the XTYPE and YTYPE keywords
;	       are used.  If these aren't specified, then a linear-linear
;	       plot is produced.  This keyword is available to maintain
;	       compatibility with the previous version of OPLOTERR.
;	NOHAT     = if specified and non-zero, the error bars are drawn
;	            without hats.
;	HATLENGTH = the length of the hat lines used to cap the error bars.
;	            Defaults to !D.X_VSIZE / 100).
;	ERRTHICK  = the thickness of the error bar lines.  Defaults to the
;	            THICK plotting keyword.
;	ERRSTYLE  = the line style to use when drawing the error bars.  Uses
;	            the same codes as LINESTYLE.
;
; RESTRICTIONS:
;	Arrays must not be of type string.  There must be enough points to
;	plot.
; PROCEDURE:
;	A plot of X versus Y with error bars drawn from Y - YERR to Y + YERR
;	and optionally from X - XERR to X + XERR is written to the output device
; MODIFICATION HISTORY:
;	Adapted from the most recent version of PLOTERR.  M. R. Greason,
;   Hughes STX, 11 August 1992. Further modified by PJS 5/13/93
if n_params(0) eq 0 then begin
print,'PLOTLC_1,flist,num=num'
print,'Plot light curves for one or more data sets; set num=1 for the first plot'
retall
end
;			Check the parameters.
;
if n_elements(num)eq 0 then num=1
xx=flist.tbin & yy=flist.flx & xerr=flist.Dt & yerr=flist.err
;
;			Interpret the keyword parameters.
plot_keywords, BACK=back,CHAN=chan,CHARSIZE=chsiz,CHARTHICK=chthck, $
	COLOR=color,DATA=data,DEVICE=device,FONT=font,LINESTYLE=linest, $
	NOCLIP=noclip,NODATA=nodata,NOERASE=noerase,NORMAL=normal,NSUM=nsum, $
	PSYM=psym,SUBTITLE=subtit,SYMSIZ=symsiz,T3D=t3d, $
	THICK=thick,TICKLEN=ticklen,TITLE=title,XCHARSIZE=xchsiz,XMARGIN=xmargn, $
	XMINOR=xminor,XRANGE=xrange,XSTYLE=xstyle,XTICKLEN=xtickln,$
	XTICKNAME=xticknm,XTICKS=xticks,XTICKV=xtickv,XTITLE=xtitle, $ 
        XTYPE=xtype,YCHARSIZE=ychsiz,YMARGIN=ymargn,YMINOR=yminor, $
        YRANGE=yrange, YSTYLE=ystyle,YTICKLEN=ytickln,	YTICKNAME=yticknm, $
        YTICKS=yticks, YTICKV=ytickv,YTITLE=ytitle,YTYPE=ytype
;
;				Error bar keywords (except for HATLENGTH; this
;				one will be taken care of later, when it is
;				time to deal with the error bar hats).
;
IF (keyword_set(hat)) THEN hat = 0 ELSE hat = 1
IF (n_elements(eth) EQ 0) THEN eth = thick
IF (n_elements(est) EQ 0) THEN est = 0
;
;				Other keywords.
;
IF (keyword_set(itype)) THEN BEGIN
	CASE (itype) OF
		   1 :  ytype = 1	; X linear, Y log
		   2 :  xtype = 1	; X log, Y linear
		   3 :  BEGIN		; X log, Y log
			xtype = 1
			ytype = 1
			END
		ELSE : 
	ENDCASE
ENDIF
n = N_elements(xx) & np=4

;	 If no y-range was passed via keyword or system variable, force one 
;	 large enough to display all the data and the entire error bars.
;	 If a reversed y-range was passed, switch ylo and yhi.
;
ylo = yy - yerr
yhi = yy + yerr
IF yrange(0) EQ yrange(1) THEN $
	yrange = [min(ylo), max(yhi)]
IF yrange(0) GT yrange(1) THEN BEGIN
	ylo = yy + yerr
	yhi = yy - yerr
ENDIF
;       
;        Similarly for x-range
;
if NP EQ 4 then begin
   xlo = xx - xerr
   xhi = xx + xerr
   IF xrange(0) EQ xrange(1) THEN xrange = [min(xlo), max(xhi)]
   IF xrange(0) GT xrange(1) THEN BEGIN
      xlo = xx + xerr
      xhi = xx - xerr
   ENDIF
endif
;
;			Plot the positions.
;
if num eq 1 then plot,xx,yy,$
        BACK=back,CHAN=chan,CHARSIZE=chsiz,CHARTHICK=chthck, $
	COLOR=color,DATA=data,DEVICE=device,FONT=font,LINESTYLE=linest, $
	NOCLIP=noclip,NODATA=nodata,NOERASE=noerase,NORMAL=normal,NSUM=nsum, $
	PSYM=psym,SUBTITLE=subtit,SYMSIZ=symsiz,T3D=t3d, $
	THICK=thick,TICKLEN=ticklen,TITLE=title,XCHARSIZE=xchsiz,XMARGIN=xmargn, $
	XMINOR=xminor,XRANGE=xrange,XSTYLE=xstyle,XTICKLEN=xtickln,$
	XTICKNAME=xticknm,XTICKS=xticks,XTICKV=xtickv,XTIT='Time, s',$
        XTYPE=xtype, $
	YCHARSIZE=ychsiz,YMARGIN=ymargn, YMINOR=yminor,YRANGE=yrange, $
        YSTYLE=ystyle,YTICKLEN=ytickln,	YTICKNAME=yticknm,YTICKS=yticks, $ 
        YTICKV=ytickv,YTITLE='Cnts/s',YTYPE=ytype,XTICK_GET=xtget,$
        YTICK_GET=ytget else oplot,xx,yy,PSYM=psym,LINESTYLE=linest
;
;       Plot the error bars.   Compute the hat length in device coordinates
;       so that it remains fixed even when doing logarithmic plots.
;
    data_low = convert_coord(xx,ylo,/TO_DEVICE)
    data_hi = convert_coord(xx,yhi,/TO_DEVICE)
    if NP EQ 4 then begin
       x_low = convert_coord(xlo,yy,/TO_DEVICE)
       x_hi = convert_coord(xhi,yy,/TO_DEVICE)
    endif
    ycrange = !Y.CRANGE   &  xcrange = !X.CRANGE
    
FOR i = 0, (n-1) DO BEGIN

    if (xtype EQ 0) then begin
       if (xcrange(0) le xcrange(1)) then begin
          if (xx(i) LT xcrange(0)) or (xx(i) GT xcrange(1)) then goto,NOPLOT
       endif else begin
          if (xx(i) GT xcrange(0)) or (xx(i) LT xcrange(1)) then goto,NOPLOT
       endelse
    endif
    if (ytype EQ 0) then begin
       if (ycrange(0) le ycrange(1)) then begin
          if (yy(i) LT ycrange(0)) or (yy(i) GT ycrange(1)) then goto,NOPLOT
       endif else begin
          if (yy(i) GT ycrange(0)) or (yy(i) LT ycrange(1)) then goto,NOPLOT
       endelse
    endif
    plots, [xx(i),xx(i)], [ylo(i),yhi(i)], LINESTYLE=est,THICK=eth
;                                                         Plot X-error bars 
;
    if np EQ 4 then plots, [xlo(i),xhi(i)],[yy(i),yy(i)],LINESTYLE=est,THICK=eth
	IF (hat NE 0) THEN BEGIN
		IF (N_elements(hln) EQ 0) THEN hln = !D.X_VSIZE/100. 
		exx1 = data_low(0,i) - hln/2.
		exx2 = exx1 + hln
		plots, [exx1,exx2], [data_low(1,i),data_low(1,i)],  $
                      LINESTYLE=est,THICK=eth,/DEVICE
		plots, [exx1,exx2], [data_hi(1,i),data_hi(1,i)],  $
                       LINESTYLE=est,THICK=eth,/DEVICE
;                                          
                IF np EQ 4 THEN BEGIN
                   IF (N_elements(hln) EQ 0) THEN hln = !D.Y_VSIZE/100.
                   eyy1 = x_low(1,i) - hln/2.
                   eyy2 = eyy1 + hln
                   plots, [x_low(0,i),x_low(0,i)], [eyy1,eyy2], $
                         LINESTYLE=est,THICK=eth,/DEVICE
                   plots, [x_hi(0,i),x_hi(0,i)], [eyy1,eyy2], $
                         LINESTYLE=est,THICK=eth,/DEVICE
                ENDIF
	ENDIF
    NOPLOT:
ENDFOR
;
RETURN
END
