pro plot_box,cor,hdr,bra,bdec,dmx,s_c=s_c,angle=angle,dmy=dmy,bhdr=bhdr,linestyle=linestype,thick=thick
;+
; plot a box in an existing 2-D plot
; bra, bdec - the RA and Dec of the box center (deg_
; dmx, dmy - the x and y dimensions of the box (arcmin). dmy = dmx if dmy is
;	not specified
; cor - normalized plot corner from cont_grey
; hdr - the header of the plot
; s_c - color of the box
; angle - clockwise (deg)
; bhdr - the header of the box region; if supplied, it is used for the box info
;
; written by wqd, 5/30/2001
;-
if N_params() lt 5 and n_elements(bhdr) eq 0 then begin ;Sufficient parameters?
  print,'CALLING SEQUENCE -plot_box,cor,hdr,bra,bdec,dmin,s_c=s_c,angle=angle,dmy=dmy,bhdr=bhdr,linestyle=linestype,thick=thick'
 return
endif

if n_elements(s_c) eq 0 then s_c=!d.n_colors-1

if n_elements(bhdr) ne 0 then begin
    box_radec,bhdr,sra,sdec
endif else begin
    if n_elements(dmy) eq 0 then dmy=dmx
    hdmx=dmx/2.
    hdmy=dmy/2.
    if n_elements(angle) eq 0 then begin
	xp=hdmx*[1,1,-1,-1,1]
	yp=hdmy*[-1,1,1,-1,-1]
    endif else begin
	xpo=hdmx*[1,1,-1,-1,1]
	ypo=hdmy*[-1,1,1,-1,-1]
	an=angle*(!pi/180.)
	xp=xpo*cos(an)+ypo*sin(an)
	yp=-xpo*sin(an)+ypo*cos(an)
    endelse
    trans_loct,xp,yp,bra,bdec,sra,sdec,pixsize=60.,/deg
endelse
source_plot,'',cor,hdr,sra=sra,sdec=sdec,psym=0,symsize=2,s_c=s_c,linestyle=linestype,thick=thick
return
end
