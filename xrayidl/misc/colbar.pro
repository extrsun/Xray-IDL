PRO COLBAR,COL
;**************************************************************************
;
;      to load a color bar and color table on the GPX. Puts a color bar on the
;      right edge of the display. Width is 20 pixels, each color 2 pixels high. 
;
;
;      First an array of width 20 and height 512 containing values 0 to
;      (20*512)/40 = 255.  The important idea to grasp is that we have a
;      color bar of width 20 and height 512 containing values 0 to 255, one
;      value every two rows {i.e. the first two rows are filled with 0's, the
;      next two rows are filled 1's,..., rows 511 & 512 are filled with
;      255's}. 
;
;      written by Michael Van Steenberg and Joel Parker     10/87
;      modified to work on GPX display by Jon Saken        3/5/91
;      modified for IDL V2 by Jon Saken                    11/12/91
;**************************************************************************
  factor = (20.*500.)/!d.n_colors
  Bar = byte(indgen(20,500)/factor)
;  if windnum eq -1 then begin
;	print,'** All Windows in Use!  Delete one and try again. **'
;	return
;  endif
;
  oldwind  = !d.window
  oldorder = !order
  if n_params(0) eq 0 then $
             window,free=1,xs=20,ys=500,/re $
  else begin
             window,free=1,xs=500,ys=20,/re
             bar = transpose(bar)
  endelse
;
  !order = 1
  tv,bar
  wset,oldwind
  !order = oldorder
;
RETURN
END

