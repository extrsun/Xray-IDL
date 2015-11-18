pro label_plot,hdr,regfile,corner=corner,charsize=charsize, $
  ctype=ctype,color=color
;+
; draw texts defined in a 'DS9' type region file. The plot area is 
; inherited from an existing plot (as defined by cor and hdr, for 
; example by cont_grey)
; 
; corner - normalized plot corner from cont_grey
; hdr    - the header of the plot
; regfile - region file name, created by ds9 or sou_final, for example
; ctype - coordinate type (def='fk5'), but it will be overwritten 
;         if the region file header has specific definition
; charsize, color - used as in 'xyouts'
; 
; May be written by sw, Jan. 4, 2011 (I cannot remember!)
; Refined by sw, Dec. 23, 2012 in some data directory
; Modified by sw, Dec. 23, 2012, add keyword 'ctype' to adjust 
;   region file stored in physical coordinate, and 'color'
;-

if keyword_set(corner) eq 0 then corner=[0,1,0,1]
if keyword_set(ctype) eq 0 then ctype='fk5'

text=''
openr,lun,regfile,/get_lun
while eof(lun) eq 0 do begin
  readf,lun,text
  str=strmid(text,0,3)
  if str eq 'phy' or str eq 'fk5' then ctype=gettok(text,';')
  ;Only region file stored as 'DS9' type could retain text note, and 
  ;the 'DS9' type contains header indicating the coordinate type, which 
  ;promises those upper words work.
  
  if (strlen(text) gt 10) then begin
    tem_arr=strsplit(text,'()',/extract)
  if (tem_arr[0] eq '# text') then begin
    te=gettok(text,'(')
    xs=gettok(text,',')
    ys=gettok(text,')')
    te=gettok(text,'{')
    la=gettok(text,'}')
    
    naxis=sxpar(hdr,'naxis*')
    if ctype eq 'fk5' then begin
      xdims=naxis(0)*abs(sxpar(hdr,'cdelt1'))
      ydims=naxis(1)*abs(sxpar(hdr,'cdelt2'))
      xmid=corner(0)+(corner(1)-corner(0))*sxpar(hdr,'crpix1')/float(naxis(0))
      ymid=corner(2)+(corner(3)-corner(2))*sxpar(hdr,'crpix2')/float(naxis(1))
      xnorm=(corner(1)-corner(0))/(xdims)
      ynorm=(corner(3)-corner(2))/(ydims)
      del_xd=xs-sxpar(hdr,'crval1')
      del_yd=ys-sxpar(hdr,'crval2')
      xsn=xmid-del_xd*cos(ys*!pi/180.)*xnorm
      ysn=ymid+del_yd*ynorm
    endif else begin
      xsn=corner[0]+(cor[1]-cor[0])*xs/naxis[0]
      ysn=corner[2]+(cor[3]-cor[2])*ys/naxis[1]
    endelse

    xyouts,xsn,ysn-0.018,la,/normal,alignment=0.8,charsize=charsize,color=color
  endif
  endif
endwhile

end

