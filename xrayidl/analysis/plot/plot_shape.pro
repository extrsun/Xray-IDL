pro plot_shape,spara,corner,hdr,gradec,nshape=nshape,linestyle=linestyle,color=color,sel=sel,fill=fill,thick=thick,noplot=noplot,pixsize=pixsize,nsign=nsign,nd=nd,ecolor=ecolor
;+
; plot an region (shaped defined by 'nshape' parameter) in an exiting plot
; 
; spara - vector containging shape parameters
;         for nshape=0 - circle: spara=[radius]; units: arcmin
;         for nshape=1 - box: spara=[xdim,ydim,angle]; 
;         for nshape=2 - ellipse: spara=[major axis,ellipticity,position angle]
;                        all dimensions are in units of arcmin; angles
;                        in degrees and anti-clockwise
;         for nshape=3  - polycon
;         for nshape=4 - circular annulus: spara=[inner radius, outer
;                        radius] ; units: arcmin
;         for nshape=5 - elliptical annulus:  spara=[[inner major
;                        axis,ellipticity],[outer major axis, 
;                        ellipticity],position angle]
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
; nsign  - if set and larger than 0, the shape is plot as a excluded one
; ecolor - color used to draw exclusive lines (default: 255)
;
; written by wqd, 6/17/03
; revised to add circular and elliptical annulus options, wqd, 7/17/07
; modified by ljt, 08/01/2008, add pixsize factor
; modified by sw, 03/25/2011, add keyword 'nsign'
; modified by sw, 12/19/2012, add keyword 'nd' (seems used but not added)
; modified by sw, 03/07/2013, add keyword 'ecolor' 
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - plot_shape,spara,corner,hdr,gradec,nshape=nshape,nsign=nsign'
print,',linestyle=linestyle,color=color,sel=sel,fill=fill,thick=thick,noplot=noplot'
return
endif
if n_elements(nshape) eq 0 then nshape=0 ;circle
if n_elements(nd) eq 0 then nd=500
if n_elements(nsign) eq 0 then nsign=0
an=(findgen(nd+1)-nd/8.)*(2.*!pi/nd)
   
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
4: begin ;circle annulus
    rs=spara(1)*60.
    xs=rs*cos(an)
    ys=rs*sin(an)
   end
5: begin ;outer boundary of the elliptical annulus
    xxe=(float(spara(2))*60.)*cos(an)
    yye=(1.-spara(3))*(float(spara(2))*60)*sin(an)
    if n_elements(spara) eq 5 then begin
        rpa=spara(4)*!pi/180. 
         ; get to the plot coordinates
        xs=xxe*sin(rpa)+yye*cos(rpa)
        ys=-xxe*cos(rpa)+yye*sin(rpa)
    endif else begin
        xs=xxe & ys=yye
    endelse 
    end
else: begin ;to be added
    print,'shape not implemented yet'
    end
endcase

if n_elements(gradec) ne 0 then begin 
    crval=sxpar(hdr,'crval*',count=nc)
    crpix=sxpar(hdr,'crpix*',count=nc2)
    ; print,crval
    if nc gt 2 or nc2 gt 2 then $
      print,'more than 2 crval parameters in the header!!'
    trans_dist,crval(0),crval(1),gradec(0),gradec(1),xp,yp,/das,/deg,pixsize=pixsize
    xs=xs+xp
    ys=ys+yp
endif 
; xs and ys are the distance of the points used to plot the region (in 
; arcsec) to the reference point (crval*) in the RA and Dec direction, 
; respectively. For RA, to right is positive; for Dec, to top is positive. 

plot_add,hdr,corner,xs,ys,linestyle=linestyle,color=color,fill=fill, $
  thick=thick,sel=sel,noplot=noplot,nsign=nsign,ecolor=ecolor
if nshape ne 4 or nshape ne 5 then return

;plot the inner boundaries for annulus:
case nshape of
4: begin ;circle annulus
    rs=spara(0)*60.
    xs=rs*cos(an)
    ys=rs*sin(an)
   end
5: begin ;elliptical annulus
    xxe=(float(spara(0))*60.)*cos(an)
    yye=(1.-spara(1))*(float(spara(0))*60)*sin(an)
    if n_elements(spara) eq 5 then begin
        rpa=spara(4)*!pi/180. 
         ; get to the plot coordinates
        xs=xxe*sin(rpa)+yye*cos(rpa)
        ys=-xxe*cos(rpa)+yye*sin(rpa)
    endif else begin
        xs=xxe & ys=yye
    endelse 
    end
else:
endcase
if n_elements(crval) ne 0 then begin 
    xs=xs+xp
    ys=ys+yp
endif 
plot_add,hdr,corner,xs,ys,linestyle=linestyle,color=color,fill=fill, $
  thick=thick,sel=seln,noplot=noplot,nsign=nsign
match,sel,seln,suba,count=count
if count ne 0 then remove,suba,sel
if !debug eq 3 then stop
return
end

