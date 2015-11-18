pro sh_plot_log,x,flux,eflux,mflux,title=title,xfactor=xfactor,yfactor=yfactor, $
 errl=errl,erru=erru,xrange=xrange,yrange=yrange,xytitle=xytitle,mflux2=mflux2 $
	,ytitle=ytitle,xtitle=xtitle,flo=flo,fhi=fhi,pthick=pthick,psym=psym, charsize=charsize
;** Plot publishable graphs of data (flux) with error bars (eflux) and
;** a model of the data (mflux); the dependent variable is x.
;** INPUTS:
;**   x          == dependent variable data to be plotted;
;**   flux       == independent variable data to be plotted;
;**   eflux      == error bars on the dependent variable data x;
;**   mflux      == model of the data to overplot;
;**   mflux2     == second model of the data to overplot;
;**   title      == title of plot placed above plotting axes; superseded
;**                 by xytitle option;
;**   x/y-factor == factors to multiply dependent and independent data by
;**                 before plotting;
;**   errl       == lower error bars for the dependent variable data x;
;**   erru       == upper error bars for the dependent variable data x;
;**   xrange     == range of x-values to fix plot x-axis;
;**   xytitle    == 2-element array containing [x,y] coordinates for placing
;**                 title of plot inside the plot axes instead of above them.
;** Modified by kachun 28 May, 1994; added errl and erru for plotting
;**   error bars in the x-direction; added in option to use xstyle to fix
;**   ends of x-axis at user defined points.
;** Modified by kachun  June, 1994; added xytitle option to use xyouts
;**   to place title inside plot axes.  For ps-type portrait plots with
;**   xyouts charsize=2., the x-coordinate should be (15%*size of the x-axis)
;**   to the left of the right edge, and the y-coordinate should be (27%*
;**   size of the y-axis) below the top edge of the plot axes.
;** Modified by kachun 26 June, 1994; added mflux2 for overplotting a second
;**   model.
if n_params() eq 0 then begin
  print,'CALLING SEQUENCE - sh_plot_log,x,flux,eflux,mflux,title=title,'
  print,'  xfactor=xfactor,yfactor=yfactor,errl=errl,erru=erru'
  print,',xrange=xrange,xytitle=xytitle,mflux2=mflux2,ytitle=ytitle,xtitle=xtitle,flo=flo,fhi=fhi,pthick=pthick,psym=psym, charsize= charsize'
  return
endif
if n_elements(title) eq 0 then title = ''
if n_elements(xfactor) eq 0 then xfactor = 1.
if n_elements(yfactor) eq 0 then yfactor = 1.
;** Figure range in x if errl and erru are present:
if n_elements(errl) ne 0 and n_elements(erru) ne 0 and n_elements(xrange) eq 0 $
 then begin
  xplrange=[min(errl),max(erru)]
endif else if n_elements(xrange) ne 0 then begin
  xplrange=xrange
endif else xplrange=[min(x*xfactor),max(x*xfactor)]

if n_elements(charsize) eq 0 then charsize=1.2
if n_elements(pthick) eq 0 then !p.thick=2.75 else !p.thick=pthick
;if !p.multi(2) eq 2 then !p.charsize= charsize else !p.charsize=1.75
!p.charthick=1.75
factor_string,yfactor,fytitle

if n_elements(xtitle) eq 0 then begin
	factor_string,xfactor,fxtitle
	xtitle='!6IRAS 100 !7l!6m Intensity '+fxtitle+' (MJy sr!U-1!N)!6'
endif

if n_elements(ytitle) eq 0 then begin
 if !p.multi(0) eq 0 then $
; ytitle='!6 PSPC R1L Band Intensity!C('+fytitle+'ct s!U-1!N arcmin!U-2!N)!6' $
; else ytitle='!6 PSPC R2 Band Intensity!C('+fytitle+'ct s!U-1!N arcmin!U-2!N)!6'
 ytitle='!6 X-ray Intensity!6('+fytitle+'ct s!U-1!N arcmin!U-2!N)!6' $
 else ytitle='!6 X-ray Intensity!6('+fytitle+'ct s!U-1!N arcmin!U-2!N)!6'
endif

if n_elements(flo) eq 0 then flo=(flux-eflux)
if n_elements(fhi) eq 0 then fhi=(flux+eflux)

if n_elements(yrange) eq 0 then yrange=[min(yfactor*flo),max(yfactor*fhi)]
if n_elements(xrange) ne 0 then begin
  if n_elements(xytitle) ne 0 then begin
    plot_io,x*xfactor,flux*yfactor,xrange=xplrange,xstyle=1, $
      yrange=yrange,xtitle=xtitle,ytitle=ytitle,xthick=2.,ythick=2. $
      ,zthick=2.,charsize=charsize,psym=3
    xyouts,xytitle(0),xytitle(1),title,charsize=charsize,charthick=2.
  endif else begin
    plot_io,x*xfactor,flux*yfactor,xrange=xplrange,xstyle=1, $
      yrange=yrange,charsize=charsize, $
      title='!6'+title,xtitle=xtitle,ytitle=ytitle,xthick=2.,ythick=2.,zthick=2.,psym=3
  endelse
endif else begin
  if n_elements(xytitle) ne 0 then begin
    plot_io,x*xfactor,flux*yfactor,xrange=xplrange, $
      yrange=yrange,charsize=charsize, $
      xtitle=xtitle,ytitle=ytitle,xthick=2.,ythick=2.,zthick=2.,psym=3
    xyouts,xytitle(0),xytitle(1),title,charsize=charsize,charthick=2.
  endif else begin
    plot_io,x*xfactor,flux*yfactor,xrange=xplrange, $
      yrange=yrange,charsize=charsize, $
      title='!6'+title,xtitle=xtitle,ytitle=ytitle,xthick=2.,ythick=2.,zthick=2.,psym=3
  endelse
endelse
if n_elements(psym) eq 0 then psym=7

oplot,x*xfactor,yfactor*flux,psym=psym
if n_elements(mflux2) ne 0 then oplot,x*xfactor,mflux2*yfactor,linestyle=2
if n_elements(mflux) ne 0 then oplot,x*xfactor,mflux*yfactor


;** Plot error bars:
errplot,x*xfactor,yfactor*flo,yfactor*fhi
if n_elements(errl) ne 0 and n_elements(erru) ne 0 then begin
  errplot_y,flux*yfactor,errl,erru
endif

!p.thick=1.
!p.charsize=1.
!p.charthick=1.
if !debug eq 2 then stop
return
end

;**
pro factor_string,factor,ftitle
;**
;** Make string for axes titles depending.
;** Written by kachun 27 September, 1993.
if n_elements(factor) eq 0 then factor = 1.

if factor eq 1.e-5 then ftitle = '!610!U5!N!6 '
if factor eq 1.e-4 then ftitle = '!610!U4!N!6 '
if factor eq 1.e-3 then ftitle = '!610!U3!N!6 '
if factor eq 1.e-2 then ftitle = '!610!U2!N!6 '
if factor eq 1.e-1 then ftitle = '!610!U1!N!6 '
if factor eq 1.e1 then ftitle = '!610!U-1!N!6 '
if factor eq 1.e2 then ftitle = '!610!U-2!N!6 '
if factor eq 1.e3 then ftitle = '!610!U-3!N!6 '
if factor eq 1.e4 then ftitle = '!610!U-4!N!6 '
if factor eq 1.e5 then ftitle = '!610!U-5!N!6 '
if factor eq 1. then ftitle = ''

return
end
