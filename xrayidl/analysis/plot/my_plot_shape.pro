pro my_plot_shape,spara,corner,hdr,gradec,nshape=nshape,linestyle=linestyle,color=color,sel=sel,fill=fill,thick=thick,noplot=noplot
;+
; plot an ellipse in an exiting plot
; 
; spara - vector containging shape parameters
;         for nshape=0 - circle: spara=[radius]; units: arcmin
;         for nshape=1 - box: spara=[xdim,ydim,angle]; 
;         for nshape=2 - ellipse: spara=[major axis,ellipticity,position angle]
;         for nshape=3 - polycon????
;         for nshape=4 - line: spara=[length, position angle, center relative to the image center(xcenter,ycenter)]
;         for nshape=5 - arc: spara=[arcradius, center relative to the image center(xcenter,ycenter), angle range (anglebegin,angleend)]
;                        all dimensions are in units of arcmin; angles
;                        in degrees and anti-clockwise
; corner - plot position from cont_grey
; hdr - fits header of the image
; gradec - the ra and dec of the ellipse if not coinciding with the
;          image center (deg)
; linestyle, thick, color - curve style, thickness, and color 
; sel - selected image pixel index within the ellipse
; fill - if set, filling of the ellipse will be performed
; noplot - if set, no plot, only the selection of the sources
;
; written by wqd, 6/17/03
; modified by ljt, 2/25/07, add line shape
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - plot_shape,spara,corner,hdr,gradec,nshape=nshape'
print,',linestyle=linestyle,color=color,sel=sel,fill=fill,thick=thick,noplot=noplot'
return
endif
if n_elements(nshape) eq 0 then nshape=0 ;circle
if n_elements(nd) eq 0 then nd=500
an=findgen(nd+1)*(2.*!pi/nd)
   
case nshape of
0: begin ;circle
    rs=spara(0)*60.
    xs=rs*cos(an)
    ys=rs*sin(an)
   end
1: begin ;box 
    dmx=spara(0)
    if n_elements(spara) eq 1 then dmy=dmx else dmy=spara(1)
    hdmx=dmx/2.*60.         ; in arcsec
    hdmy=dmy/2.*60.         ; in arcsec
    xpo=hdmx*[1,1,-1,-1,1]
    ypo=hdmy*[-1,1,1,-1,-1]
    if n_elements(spara) eq 3 then begin  
        an=spara(2)*(!pi/180.)
        xs=xpo*cos(an)-ypo*sin(an)
        ys=xpo*sin(an)+ypo*cos(an)
    endif else begin
        xs=xpo
        ys=ypo
    endelse
  end
2: begin ;ellipse
    if n_elements(spara) lt 3 then begin
        print,'ellipse needs add least two parameters'
        ;(half major axis ellipticity)
        return
    endif 
    xxe=(float(spara(0))*60.)*cos(an)
    yye=(1.-spara(1))*(float(spara(0))*60)*sin(an)
    if n_elements(spara) eq 3 then begin
        rpa=spara(2)*!pi/180. 
         ; get to the plot coordinates
        xs=xxe*sin(rpa)+yye*cos(rpa)
        ys=-xxe*cos(rpa)+yye*sin(rpa)
    endif else begin
        xs=xxe & ys=yye
    endelse 
   end
3: begin ;polycon
    xs=spara(0,*)*60. & ys=spara(1,*)*60.
     ;relative to the original image center, to be called by plot_reg
    end
4: begin ;line
    symlength=spara(0)*60.
    posiangle=spara(1)*!pi/180.
    xcenter=spara(2)*60.
    ycenter=spara(3)*60.
    pointarr=findgen(nd)
    xs=symlength*(pointarr/float(nd)-0.5)*cos(posiangle)+xcenter
    ys=symlength*(pointarr/float(nd)-0.5)*sin(posiangle)+ycenter
    end
5: begin ;arc
    arcradius=spara(0)*60.
    xcenter=spara(1)*60.
    ycenter=spara(2)*60.
    posianglebegin=spara(3)*!pi/180.
    posiangleend=spara(4)*!pi/180.
    anglearr=findgen(nd)
    anglearr=posianglebegin+(posiangleend-posianglebegin)/float(nd)*anglearr
    xs=arcradius*cos(anglearr)+xcenter
    ys=arcradius*sin(anglearr)+ycenter
    end
6: begin ;to be added
    print,'shape not implemented yet'
    end
endcase
if n_elements(gradec) ne 0 then begin 
    crval=sxpar(hdr,'crval*',count=nc)
    if nc gt 2 then print,'more than 2 crval parameters in the header!!'
    trans_dist,crval(0),crval(1),gradec(0),gradec(1),xp,yp,/das,/deg
    xs=xs+xp
    ys=ys+yp
endif 
plot_add,hdr,corner,xs,ys,linestyle=linestyle,color=color,fill=fill,thick=thick,sel=sel,noplot=noplot
return
end

