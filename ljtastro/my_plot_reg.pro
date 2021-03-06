pro my_plot_reg,cor,hdr,regfile,crval=crval,linestyle=linestyle,thick=thick,color=color,factor=factor,selv=selv,negselv=negselv,fill=fill,noplot=noplot
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
; factor - the magnification factor to be applied to the size of the regions
; linestyle, thick, color - curve style, thickness, and color 
; selv - selected image pixel index within positive regions
; negselv - selected image pixel index within negative regions
; fill - if set, filling of the ellipse will be performed
; noplot - if set, no plot, only the selection of the sources
;
; written by wqd, June 4, 2003
;-
if N_params() eq 0 then begin
  print,'CALLING SEQUENCE - plot_reg,cor,hdr,regfile,crval=crval'
  print,',linestyle=linestype,thick=thick,color=color,factor=factor'
  print,',selv=selv,negselv=negselv,fill=fill,noplot=noplot'
  return
endif
if n_elements(color) eq 0 then color=!d.n_colors-1
if n_elements(crval) eq 0 then crval=sxpar(hdr,'crval*')

selv=[999L]
negselv=[999L]
openr,un,regfile,/get
while eof(un) eq 0 do begin
  read_reg,un,crval,nshape,sra,sdec,spara,nsign,factor=factor
  print,"my_plot_reg:", spara,sra,sdec
  my_plot_shape,spara,cor,hdr,gradec,nshape=nshape,linestyle=linestyle $
    ,color=color,sel=sel,fill=fill,thick=thick,noplot=noplot
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
