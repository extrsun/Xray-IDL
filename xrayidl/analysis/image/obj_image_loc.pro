pro obj_image_loc,hdr,sra,sdec,loc,sel=sel,xxp=xxp,yyp=yyp,im=im
;+
; get image pixel locations of objects 
;
; hdr - header of the image
; sra, sdec - Ra dn Dec of the objects in deg
; loc - image pixel location
; sel - index of the objects that are within the image
; xxp, yyp - x and y coodinates of the selected objects (in fortran format)
; im - the image constructed from the objects
;
; written by wqd, 1/23/04
; revised to have xxp and yyp in the fortran format, wqd, 5/16/04
;-
if n_params() eq 0 then begin
print,"Calling Seq -  obj_image_loc,hdr,sra,sdec,loc,sel=sel,xxp=xxp,yyp=yyp,im=im'
return
endif

get_headinfo,hdr,xsize,ysize,cra,cdec,cpx,cpy,delx,dely
if abs(dely+delx) gt 1.e-6  then print,'***WARNING: delx not = dely: ',delx,dely
trans_dist,cra,cdec,sra,sdec,xp,yp,/deg,pixsize=dely*3600.0d

; get source pixel locations in the image:
yyp=nint(yp+cpy)
xxp=nint(xp+cpx)
sel=where(xxp ge 1 and xxp le xsize and yyp ge 1 and yyp le ysize,nsel)
if nsel ne 0 then begin
    xxp=xxp(sel)
    yyp=yyp(sel)
    loc=long(yyp-1)*xsize+long(xxp-1)
endif else print,'nsel = 0!'
list_image,0,1,1,im,xsize,ysize,block=1,xp=xxp,yp=yyp
return
end
