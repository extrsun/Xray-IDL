;+
; NAME:
;	GREY
; PURPOSE:
;	Plot grey scale image in normalized coordinates. Overlaid contours 
; can be obtained by calling cont_grey instead
;
; CATEGORY:
;	General graphics.
; CALLING SEQUENCE:
;	GREY, a
; INPUTS:
;	A = 2 dimensional array to display as a greyscale plot
;
; KEYWORD PARAMETERS:
;	/nx1,/nx2,/ny1,/ny2  = The coordinates of the lower left and the upper
;                            right coners of the image
;	/NOASPECT = set to retain image's dimension ratio.  Otherwise,
;	the image's aspec ratio is retained. Assumes square
;		pixels.  
;	/INTERP = set to bi-linear interpolate if image is resampled.
;       /COLORS = color desired for contour plot. 
;	rimage - if non zero, the grey scale will be generated with a
;	random generator (using plot_rimage). The value of rimage is used
;	as the maximum expected number points per pixel
;
;	pscolor - a keyword if set indicates a color postscript file is to
;		be produced.
;	barshiftx, barshifty, barfactor - the x and y shifts (in normalized
;	  coordinates) and multiple size factor of the color bar (default=1;
;	  the same linear size as the figure). These three parameters are
;	  used to locate the bar in the figure.
; barscale - if set, the scales of the color-bar is presented. 
;   NOTICE: Only works when the keyword 'noscale' is not set, 
;      which indicates that the greyscale is calculated by this 
;      procedure instead of user's preset. In the later case, the 
;      intesity value is missing. Recently, only works under 'PS'.
; barbrdfac - if set, the broaden factor of the color bar. 
;      Motivation: the color bar for the Position-Velocity diagram 
;      is too narrow since the PV diagram is a slim one.
; barsclvertical - if set, output the label of the color bar scale 
;      vertically and only on the right side of the color bar.
; barsclnum - number of the ticks of the scales of the color bar. 
;      def: as calculated according to the keyword "barfactor"
;	ntoppix - the number of top color indexes in the color table
;	used for other purposes (e.g., drawing  the frame of the figure)
;	If the original color table is expanded to more levels (because
;	of device differences), ntoppix may need to be increased accordingly.
;	noscale - if set, the image is not scaled by bscale.
;
; OUTPUTS:
;	No explicit outputs.
; COMMON BLOCKS:
;	none.
; SIDE EFFECTS:
;	The currently selected display is affected.
; RESTRICTIONS:
;	None that are obvious.
; PROCEDURE:
;       Use normalized coordinates instead of device coordinates
;	the plot window.
; SUBROUTINES CALLED:
; 	bscale,index_conv
; MODIFICATION HISTORY:
;     writen on Aug 19 1992 (WQD) 
; 	add the option to use the random number generator for a better
;	look of an image, Aug. 1993 (wqd)
; added keywords, pscolor, for a color postscript file
; keywords: barshiftx, barshifty, barfactor are added 
; wqd, Jan 7, 1994
; modified by sw, Aug 13, 2013: add the plot of the scale of color 
;   bar. Controled by keyword barscale
; modified by sw, Aug 29, 2013: add a keyword "barbrdfac" to broaden 
;   the color bar from 0.05 of plot width.
; modified by sw, Sep 05, 2013: add the keyword "barsclvertical" to 
;   make a special vertical labels of the scale of the color bar to 
;   fit the PV diagram plotting; add the keyword "barsclnum" to 
;   adjust the number of the scale ticks; add the keyword "barsclnotesize" 
;   to adjust the character size of the labels of the color bar scales. 
;-
pro grey, aa, NOASPECT = noaspect, INTERP = interp, COLORS  = colors, $
   device=device,rimage=rimage,greymin=greymin,greymax=greymax, $
   greylog=greylog,xbar=xbar,pscolor=pscolor,noerase=noerase, $
   corner=corner,barshiftx=barshiftx,barshifty=barshifty, $
   barfactor=barfactor,ntoppix=ntoppix,nbotpix=nbotpix, $
   noimage=noimage,b_c=b_c,noscale=noscale,nocont=nocont, $
   true=true,barscale=barscale,barbrdfac=barbrdfac, $
   barsclvertical=barsclvertical,barsclnum=barsclnum, $
   barsclnotesize=barsclnotesize

  common colors,r_orig,g_orig,b_orig,r_curr,g_curr,b_curr
  ;
  if (n_params() eq 0) then begin
    print,' grey, aa,noaspect,interp,colors,device,rimage'
    print,',greymin,greymax,greylog,xbar,pscolor,noerase,corner'
    print,',barscale,barshiftx,barshifty,barfactor,ntoppix,'
    print,'nbotpix,b_c,noimage,noscale,true]'
    Return
  endif
  ;

  ; set background color if keyword b_c is given
  if n_elements(b_c) ne 0 then begin
  	polyfill,[0,1,1,0],[0.,0.,1,1],color=b_c,/norm
  	noerase=1
  endif
  a=aa
  if n_elements(greylog) eq 0 then greylog=0
  if greylog eq 1 then a=alog10(a > 1.e-10)
  if greylog eq 2 then a=sqrt(a > 1.e-10)

  if n_elements(greymin) eq 0 then begin 
  	greyminn=min(a)
  endif else begin
  	case greylog of
  		1: greyminn=alog10(greymin)
  		2: greyminn=sqrt(greymin)
  		else: greyminn=greymin
  	endcase
  	c=where(a ne 0,nc)
  	if n_elements(nc) ne 0 then a(c)=a(c) > greyminn 
  endelse
  if n_elements(greymax) eq 0 then begin 
  	greymaxn=max(a)
  endif else begin
  	case greylog of
  		1: greymaxn=alog10(greymax)
  		2: greymaxn=sqrt(greymax)
  		else: greymaxn=greymax
  	endcase
  	a=a < greymaxn
  endelse
  if n_elements(corner) eq 0 then corner=[0.,1.,0.,1.] else corner=double(corner)
  ;nx1=corner(0) & nx2=corner(1)  ;could be overruled if noaspect is not set
  ;nx3=corner(2) & nx4=corner(3)  ;could be overruled if noaspect is not set
  if keyword_set(device) ne 0 then begin
  	dname=!d.name
  	set_plot,device
  	device,/color
  endif
  if n_elements(ntoppix) eq 0 then ntoppix=1 
  	;reserved the highest pixel value for other uses (e.g., contours)
  if n_elements(nbotpix) eq 0 then nbotpix=1 ;to make the background not white
  ;
  ;on_error,2                      ;Return to caller if an error occurs
  sz = size(a)			;Size of image
  if sz(0) lt 2 then message, 'Parameter not 2D'
  ;  
  colorsav = !color               ;save colors to restore later
  mx = !d.n_colors-1		;Brightest color
  if (!d.name ne 'PS' or keyword_set(pscolor) ne 0) $
  	and n_elements(r_curr) ne 0 then begin
  	tvlct,r_orig,g_orig,b_orig
  endif else begin
  	colors=lindgen(!d.n_colors-ntoppix) 
  	tvlct,colors,colors,colors
  ;	loadct_self,14
  endelse
  if !d.name ne 'Z' then $
    contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4,noerase=noerase
  ;
  swx=(corner(1)-corner(0))*!d.x_vsize        
  swy=(corner(3)-corner(2))*!d.y_vsize
  dxa=corner(0)*!d.x_vsize
  dya=corner(2)*!d.y_vsize
  six = float(sz(1))		;Image sizes
  siy = float(sz(2))
  aspi = six / siy		;Image aspect ratio
  aspw = float(swx) / swy		;Window aspect ratio
  f = aspi / aspw			;Ratio of aspect ratios
  ;
  if keyword_set(noaspect) ne 1 then begin	;Retain aspect ratio?
    ;Adjust window size
    if f ge 1.0 then swy = swy / f else swx = swx * f
  endif
  corner(1)=corner(0)+swx/!d.x_size
  corner(3)=corner(2)+swy/!d.y_size
  if not keyword_set(noscale) then $
    aaa=bscale(a,greyminn,greymaxn,ntoppix=ntoppix,nbotpix=nbotpix) $
      else aaa=a
  if keyword_set(noimage) ne 0 then return
  if n_elements(rimage) ne 0 then begin
    plot_rimage,aaa*(rimage/238.),corner
    goto,done
  endif

  if (!d.flags and 1) ne 0 then begin	;Scalable pixels?
    if !d.name eq 'PS' and keyword_set(pscolor) eq 0 then begin
      tv,!d.n_colors-ntoppix-aaa,dxa,dya, $
        xsize = swx, ysize = swy, /device,true=true
    endif else begin
      tv,aaa,dxa,dya,xsize = swx, ysize = swy, /device,true=true
      ; print,min(aaa),max(aaa)
    endelse
  endif else begin	;Not scalable pixels	
    if not keyword_set(true) then begin
      if !d.name eq 'Z' then $
        tv,!d.n_colors-ntoppix-poly_2d(aaa, $
          [[0,0],[six/swx,0]], [[0,siy/swy],[0,0]], $
          keyword_set(interp),swx,swy),dxa,dya,true=true $
      else $
        tv,poly_2d(aaa, $
          [[0,0],[six/swx,0]], [[0,siy/swy],[0,0]], $
          keyword_set(interp),swx,swy),dxa,dya,true=true
    endif else begin
      if abs(six-swx)+abs(siy-swy) gt 0.5 then begin
        ccc=poly_2d(aaa(*,*,0), $
          [[0,0],[six/swx,0]], [[0,siy/swy],[0,0]], $
            keyword_set(interp),swx,swy)
        sz=size(ccc)
        bbb=fltarr(sz(1),sz(2),3)
        bbb(*,*,0)=ccc
        for kk=1,2 do bbb(*,*,kk)=poly_2d(aaa(*,*,kk), $
          [[0,0],[six/swx,0]], [[0,siy/swy],[0,0]], $
            keyword_set(interp),swx,swy)
        aaa=bbb
      endif
      tv,aaa,dxa,dya,xsize = swx, ysize = swy, /device,true=true
    endelse
  endelse		
  if n_elements(barshiftx) eq 0 then barshiftx=0.02
  if n_elements(barshifty) eq 0 then barshifty=0
  if n_elements(barfactor) eq 0 then barfactor=1.
  if keyword_set(barbrdfac) eq 0 then barbrdfac=1.
  if keyword_set(barsclnotesize) eq 0 then barsclnotesize=1.0
  if barfactor ne 0 then begin ; get the color bar
    bsx=barshiftx*!d.x_size
    bsy=barshifty*!d.x_size

    ;nbin=!d.n_colors-(ntoppix+nbotpix)
    nbin=256-(ntoppix+nbotpix)
    if !d.name eq 'PS' then begin
      if n_elements(xbar) ne 0 then begin
        index_conv,lindgen((nbin)*20) $
          ,[nbin,20],index
        ; index=index+nbotpix
        bar=fltarr(nbin,20)
        if keyword_set(pscolor) eq 0 then $
          bar(*)=(nbin-index(0,*)) else $
            bar(*)=index(0,*)+nbotpix
        tv,bar,bsx+dxa,bsy+2+dya+swy,xsize =barfactor*swx, $
          ysize = 0.05*barbrdfac*swy,/device
      endif else begin
        index_conv,indgen((nbin)*20) $
          ,[20,nbin],index
        ; index=index+nbotpix
        bar=fltarr(20,nbin)
        if keyword_set(pscolor) eq 0 then $
          bar(*)=(nbin-index(1,*)) else $
            bar(*)=index(1,*)+nbotpix
        tv,bar,bsx+dxa+swx,bsy+dya+2,xsize =0.05*barbrdfac*swx, $
          ysize =barfactor*swy,/device
      endelse
      if keyword_set(noscale) eq 0 and keyword_set(barscale) then begin
        ; Define the lower-left corner of the plot
        if nbotpix lt 15 then begin
          sfactor=nbotpix/(!d.n_colors-ntoppix-nbotpix-1)
          nbotpix=0
        endif else sfactor=0.0
        afactor=sfactor+1
        if keyword_set(xbar) ne 0 then $
          devcor=[bsx+dxa-sfactor*barfactor*swx, $
                2+bsy+dya+swy-sfactor*0.05*barbrdfac*swy] $
        else $
          devcor=[bsx+dxa+swx-sfactor*0.05*barbrdfac*swx, $
                2+bsy+dya-sfactor*barfactor*swy]
        ; Define the plot range to adjust the color-bar range
        range=[nbotpix*(greymaxn-greyminn)/(!d.n_colors-ntoppix-1)+greyminn,greymaxn]
        ; Calculate the minimum integer of the scale value (one order 
        ; lower than the range[0] value)
        case greylog of
          1: begin
            lowval=10^range[0] & loworder=floor(alog10(10^range[1]-10^range[0]))
          end
          2: begin
            lowval=range[0]^2
            loworder=floor(alog10(range[1]^2-range[0]^2))
          end
          else: begin
            lowval=range[0] & loworder=floor(alog10(range[1]-range[0]))
          end
        endcase
        divfac=10.^(loworder-1) & lowscale=ceil(lowval/divfac)*divfac
        case greylog of
          1: lowscale=alog10(lowscale)
          2: lowscale=sqrt(lowscale)
          else: lowscale=lowscale
        endcase
        ; print,range,lowscale

        ; Calculate the proper ticklength and corresponding tick number
        if keyword_set(barsclnum) eq 0 then begin
          ticks=floor(barfactor*4.3+1.4) ;the original number is calculated
            ; according to the color bar length 
        endif else ticks=barsclnum
        dellen=range[1]-lowscale & flag=0
        case greylog of 
          1: if dellen lt alog10(4) then flag=1
          2: if dellen*(range[1]+lowscale) lt divfac*3 then flag=1
          else: if dellen lt divfac*3 then flag=1
        endcase
        if flag eq 1 then begin
          ticklen=floor((greymaxn-greyminn)*ticks)/ticks
          tickv=(findgen(ticks+1)+1)*ticklen/(ticks+1)
        endif else begin
          if greylog eq 2 then begin
            ticklen=floor((greymaxn-lowscale)*ticks)/ticks^2
          endif else begin
            ticklen0=dellen/ticks
            case greylog of 
              1: priori=alog10(findgen(10)+1)
              else: priori=findgen(11)*divfac
            endcase
            intlen=floor(ticklen0) & fralen=ticklen0-intlen
            pridis=abs(fralen-priori) & minpri=min(pridis,mprind)
            if intlen eq 0 and mprind eq 0 then ticklen=priori[1] $
              else ticklen=priori[mprind]+intlen
            ticks=floor(dellen/ticklen)
          endelse
          tickv=findgen(ticks+1)*ticklen+lowscale
        endelse
        ; Generate the tickname
        if divfac ge 1.0 then strfmt='(f15.0)' else begin
          lfrc=1-loworder & strfmt='(f15.'+strtrim(string(lfrc),2)+')'
        endelse
        case greylog of 
          1: tickname=strtrim(string(10^tickv,format=strfmt),2)
          2: tickname=strtrim(string(tickv^2,format=strfmt),2)
          else: tickname=strtrim(string(tickv,format=strfmt),2)
        endcase

        ; Plot the color bar scales
        if keyword_set(xbar) ne 0 then begin
          plot,range,[0,20],xrange=range,xstyle=9,yrange=[0,20], $
            position=[devcor,devcor+[barfactor*swx,0.05*barbrdfac*swy]*afactor], $
            /device,/noerase,/nodata,ystyle=1,xtickname=tickname, $
            xticks=ticks,xtickv=tickv,yticks=1, $
            ytickname=replicate('',2), $
            xticklen=0.25*barbrdfac*swy*afactor/!d.x_vsize
          axis,/xaxis,xrange=range,xstyle=1,/save,xticks=ticks, $
            xtickv=tickv,xtickname=tickname, $
            xticklen=0.25*barbrdfac*swy*afactor/!d.x_vsize
        endif else begin
          if keyword_set(barsclvertical) eq 0 then begin
            plot,[0,20],range,yrange=range,ystyle=9,xrange=[0,20], $
              position=[devcor,devcor+[0.05*barbrdfac*swx,barfactor*swy]*afactor], $
              /device,/noerase,/nodata,xstyle=1,ytickname=tickname, $
              yticks=ticks,ytickv=tickv,xticks=1, $
              xtickname=replicate(' ',2), $
              yticklen=0.25*barbrdfac*swx*afactor/!d.y_vsize
            ; print,range,ticks
            axis,/yaxis,yrange=range,ystyle=1,/save,yticks=ticks, $
              ytickv=tickv,ytickname=tickname, $
              yticklen=0.25*barbrdfac*swx*afactor/!d.y_vsize
          endif else begin
            plot,[0,20],range,yrange=range,ystyle=1,xrange=[0,20],xstyle=1, $
              position=[devcor,devcor+[0.05*barbrdfac*swx,barfactor*swy]*afactor], $
              /device,/noerase,/nodata,xticks=1,xtickname=replicate(' ',2), $
              yticks=ticks,ytickv=tickv,ytickname=replicate(' ',ticks+1), $
              yticklen=0.25*barbrdfac*swx*afactor/!d.y_vsize
            for ticn=0,ticks do $
              xyouts,devcor[0]+(0.06+barsclnotesize/50.)*barbrdfac*swx*afactor, $
                barfactor*swy*afactor*(tickv[ticn]-range[0])/(range[1]-range[0])+devcor[1], $
                tickname[ticn],alignment=0.5,orientation=90, $
                /device,charsize=barsclnotesize
          endelse
        endelse
      endif
    endif else begin
      if n_elements(xbar) ne 0 then begin
        iswx=fix(swx*barfactor)
        factor=(nbin)/(swx)
        index_conv,lindgen(iswx*20),[iswx,20],index
        bar=fltarr(iswx,20)
        bar(*)=index(0,*)*factor+nbotpix
        tv,bar,bsx+dxa,bsy+2+dya+swy
      endif else begin
        iswy=fix(swy*barfactor)
        factor=(nbin)/(swy)
        index_conv,lindgen(iswy*20L),[20,iswy],index
        bar=fltarr(20,iswy)
        bar(*)=index(1,*)*factor+nbotpix
        tv,bar,bsx+2+dxa+swx,bsy+dya
      endelse
    endelse
  endif
	
  ;
  done:
  ;!color = colorsav
  if keyword_set(device) ne 0 then begin
    device,/close
    set_plot,dname
  endif
  if !debug eq 2 then stop
  return
end
