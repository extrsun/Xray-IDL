pro plot_reg,cor,hdr,regfile,crval=crval,crpix=crpix,pixsize=pixsize,linestyle=linestyle,thick=thick,color=color,factor=factor,selv=selv,negselv=negselv,fill=fill,noplot=noplot,ctype=ctype,ecolor=ecolor
;+
; overplot the outlines of regions defined in a standard region file
; on an existing plot (as defined by cor and hdr, for example by
; cont_grey)
;
; cor - normalized plot corner from cont_grey
; hdr - the header of the plot
; regfile - region file name, created by ds9 or sou_final, for
;           example.
; crval - 2-element vector containing the RA and Dec of the
;         observation aiming point. If not given, crval in hdr is assumed.
; color - color of the plotting
; ctype - coordinate type (def='physical')
;           currently, support 'phy' or 'fk5' coordinates.
; factor - the magnification factor to be applied to the size of the regions
; linestyle, thick, color - curve style, thickness, and color 
; pixsize - pixel size in arcsec
; selv - selected image pixel index within positive regions
; negselv - selected image pixel index within negative regions
; fill - if set, filling of the ellipse will be performed
; noplot - if set, no plot, only the selection of the sources
; ecolor - color used to draw exclusive lines (default value is defined in 
;          'plot_shape.pro' as 255)
;
; written by wqd, June 4, 2003
; modified by sw, March 7, 2013, add keyword 'ecolor' to indicate color used 
;   in drawing exclusive lines
;-
if N_params() eq 0 then begin
  print,'CALLING SEQUENCE - plot_reg,cor,hdr,regfile,crval=crval,crpix=crpix'
  print,',pixsize=pixsize,linestyle=linestype,thick=thick,color=color'
  print,',factor=factor,selv=selv,negselv=negselv,fill=fill,noplot=noplot'
  print,'ctype=ctype'
 return
endif
if n_elements(color) eq 0 then color=!d.n_colors-1
if n_elements(crval) eq 0 then crval=sxpar(hdr,'crval*')
if n_elements(crpix) eq 0 then crpix=sxpar(hdr,'crpix*')
if keyword_set(ctype) eq 0 then ctype='physical'
if keyword_set(pixsize) eq 0 then pixsize=abs(sxpar(hdr,'cdelt1'))*3600.
if ctype eq 'physical' then imgcen=crpix else imgcen=crval
; print,crval,crpix,pixsize,format='(5e15.5)'

selv=[999L]
negselv=[999L]
openr,un,regfile,/get
while eof(un) eq 0 do begin
  ; print,crval
    read_reg,un,crval,nshape,sra,sdec,spara,nsign,factor=factor,ctype=ctype, $
      pixsize=pixsize,imgcen=imgcen
    ; print,spara[0:9]
    plot_shape,spara,cor,hdr,[sra,sdec],nshape=nshape,linestyle=linestyle, $
      nsign=nsign,color=color,sel=sel,fill=fill,thick=thick,noplot=noplot, $
      pixsize=!size_pixel*factor,ecolor=ecolor
    if sel(0) ne -1 then begin
        if nsign eq 0 then selv=[selv,sel] else negselv=[negselv,sel]
    endif 
if !debug eq 2 then stop
endwhile
free_lun,un
if n_elements(selv) gt 1 then selv=selv(1:*)
if n_elements(negselv) gt 1 then negselv=negselv(1:*)
return
end
