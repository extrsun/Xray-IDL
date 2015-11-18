pro filter_reg,filter,hdr,mfname=mfname,refun=refun,regfile=regfile,sname=sname
;+
; generate a spatial filter from a region file
;
; filter - output filter image. Selected regions will have values
;          equal to 1 or to the make if provided
; hdr - the fits header of the filter, which should be large enough to
;       include all the regions (e.g., the header of the mask or the
;       exposure map). If not provided, hdr will be extracted from mfname
; mfname - name of the mask file, if provided, will be used as the
;          base of the filter
; regfile - region file name. Regions to be excluded should have a "-"
;           in front the shape, e.g., physical;-circle(739.5,701.5,168.75246).
;           Positive and negative regions can be interposed.
; refun - opened region file  code number. If provided, regfile is not used.
; sname - source name
;
;Note:
; The region file can contain entries for multiple sources separated
; by an empty line. For each source, the region(s) with positive
; sign(s) will first be set to 1. The regions with negative signs will 
; THEN be set to 0 in the filter.
;
; written by wqd, July, 29, 2007
;-
if N_params() eq 0 then begin
  print,'CALLING SEQUENCE - filter_reg,filter,hdr,mfname=mfname,refun=refun,regfile=regfile,sname=sname'
 return
endif
;if n_elements(color) eq 0 then color=!d.n_colors-1
if n_elements(hdr) eq 0 then begin
    if n_elements(mfname) ne 0 then hdr=headfits(mfname) else begin
        print,'Either hdr or mfname is needed!!!'
    endelse 
endif
crval=sxpar(hdr,'crval*')

if n_elements(mfname) ne 0 then begin
    cast,mfname,hdr,outa=filtero,/noint 
endif else begin
    mdim=sxpar(hdr,'naxis*')
    filtero=fltarr(mdim(0),mdim(1))+1.
endelse 

cor=[0,1,0,1]
cont_grey,filtero,hdr,cor=cor,/full,/noc,barf=0
if n_elements(refun) eq 0 then openr,un,regfile,/get else un=refun
psel=[-999]
nsel=[-999]
send=0 
while eof(un) eq 0 and send ne 1 do begin
    read_reg,un,crval,nshape,sra,sdec,spara,nsign,factor=factor,sname=sname $
      ,texto=texto,ctype=ctype,send=send
    if send ne 1 then begin
        if nsign eq 1 then linestyle=2 else linestyle=0
        plot_shape,spara,cor,hdr,[sra,sdec],nshape=nshape,linestyle=linestyle $
          ,color=color,sel=sel,fill=fill,thick=thick,noplot=noplot
        if nsign eq 0 then psel=[psel,sel] else nsel=[nsel,sel]
    endif 
endwhile
print,'source name = ', sname
if n_elements(refun) eq 0 then free_lun,un
filter=filtero*0.
psel=psel(1:*)
filter(psel)=filtero(psel)
if n_elements(nsel) gt 1 then begin
    nsel=nsel(1:*)
    filter(nsel)=0.
endif
if !debug eq 3 then stop
return
end
