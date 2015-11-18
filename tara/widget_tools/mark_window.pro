;+
;========================================================================
;;;
;;; FILE NAME:    $Id: mark_window.pro 1458 2001-11-19 14:07:43Z patb $   
;;;
;;; DESCRIPTION:  Window Marking Routine
;;;
;;;               Xloc, Yloc, and size can be vectors or scalars.
;;;               Example usage: mark_window, 100,200, 10, WIN=0, 
;;;                                          LABEL='The Spot'
;;;
;;;               Plots a '+' at "Location" (X,Y) in device
;;;               or data coordinates.  The size is specified as a 
;;;		  multiple of the character size on the current device. 
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1993, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;-
;========================================================================
PRO mark_window, Xloc, Yloc, size, WINDOWNUM=WindowNum, DATA=data, $
		 LABEL=Label, TEXTSIZE=TextSize, COLOR=Color, BOX=Box


S = size(WindowNum)
if S(1) NE 0 then begin
  OldWindow = !d.window
  wset, WindowNum ;WindowNum defined
endif

S = size(Color)
if S(1) EQ 0 then $ ;Color undefined
  Color = !P.color

if NOT keyword_set( TextSize ) then TextSize = 2

if keyword_set(data) then begin
  point = round(convert_coord( Xloc, Yloc, /DATA, /TO_DEVICE ))
  X=point[0:0] & Y=point[1:1]
endif else begin
  X =[round(Xloc)] & Y=[round(Yloc)]
endelse

half_size = (!D.X_CH_SIZE * size) / 2


Xl = X-half_size
Xh = X+half_size
Yl = Y-half_size
Yh = Y+half_size

index = where(X GT 0 AND X LT !D.X_SIZE AND Y GT 0 AND Y LT !D.Y_SIZE, Npoints)


if keyword_set(Box) then begin
  for ii = 0L, Npoints-1 do begin
    I = index[ii]
    plots, [Xl(I), Xh(I)], [Yh(I), Yh(I)], /dev, COLOR=Color, THICK=2
    plots, [Xl(I), Xh(I)], [Yl(I), Yl(I)], /dev, COLOR=Color, THICK=2
    plots, [Xl(I), Xl(I)], [Yl(I), Yh(I)], /dev, COLOR=Color, THICK=2
    plots, [Xh(I), Xh(I)], [Yl(I), Yh(I)], /dev, COLOR=Color, THICK=2
  endfor
endif else begin
  for ii = 0L, Npoints-1 do begin
    I = index[ii]
    plots, [Xl(I), Xh(I)], [ Y(I),  Y(I)], /dev, COLOR=Color, THICK=2
    plots, [ X(I),  X(I)], [Yl(I), Yh(I)], /dev, COLOR=Color, THICK=2
  endfor
endelse

if keyword_set( Label ) then begin
  TheLabels = strarr(Npoints)
  if isarray(Label) then TheLabels(0) = string(Label) $
		    else TheLabels(*) = string(Label)
  for ii = 0L, Npoints-1 do begin
    I = index[ii]
     xyouts, X(I)-3, Y(I)+3, TheLabels(I), /dev, $
	     SIZE=TextSize, COLOR=Color, ALIGN=1.0
  endfor
endif

S = size(OldWindow)
if S(1) NE 0 then wset, OldWindow
return
end

