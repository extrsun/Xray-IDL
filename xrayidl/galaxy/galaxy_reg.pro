pro galaxy_reg,outfile,cra,cdec,gra,gdec,d25,el,pa,factor=factor,pixsize=pixsize
;+
; Produce a regional file from a source fits file (my source list)
; 
; hdr - the raw count image fits header, produced by file_params
; outfile - output regional file
; slist - source structure
; infile - the name of the source file (if slist is not provided)
; slow, flow, probth - signal-to-noise, cntr, and probability thresholds for 
; selecting the sources from infile
; radius - off-axis radius threshold
; factor - the factor for scaling the source radii (def=2)
;
; writen by WQD 12/23/2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - '
return
endif
if n_elements(factor) eq 0 then factor=1.
if n_elements(pixsize) eq 0 then pixsize=!size_pixel
trans_dist,cra,cdec,gra,gdec,xd,yd,/deg,pixsize=pixsize
xd=!pref+xd
yd=!pref+yd
major_a=d25*30./pixsize*factor
minor_a=major_a*(1.-el)

openw,outn,outfile,/get
ns=n_elements(d25)
if n_elements(pa) eq 0 then begin
    shape='circle(' 
    for k=0,ns-1 do begin
        printf,outn,shape+strtrim(xd(k),2)+','+strtrim(yd(k),2)+',' $
          +strtrim(major_a(k),2)+')'
    endfor
endif else begin
    shape='ellipse('
    for k=0,ns-1 do begin
        printf,outn,shape+strtrim(xd(k),2)+','+strtrim(yd(k),2)+',' $
          +strtrim(major_a(k),2)+',' +strtrim(minor_a(k),2)+',' +strtrim(180.-pa(k),2)+')'
    endfor
endelse 
free_lun,outn
return
end
