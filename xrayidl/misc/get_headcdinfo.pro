PRO get_headcdinfo,header,xsize,ysize,cra,cdec,cpx,cpy,del,ang,equi=equi
;+
; get information from a fits header with CD keywords
;

; ang - angle in units of degree (anti-clockwise?)
; del - plate scale in x and y directions.
;
; written by wqd, Jan, 2004 
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_headcdinfo,header,xsize,ysize,cra,cdec,cpx,cpy,delv,ang,equi=equi'
return
endif 

xsize=sxpar(header,'naxis1')
ysize=sxpar(header,'naxis2')
cra=sxpar(header,'crval1')
cdec=sxpar(header,'crval2')
cpx=sxpar(header,'crpix1')
cpy=sxpar(header,'crpix2')
cd=dblarr(2,2)
cd(*)=[sxpar(header,'CD1_1'),sxpar(header,'CD2_1'),sxpar(header,'CD1_2'),sxpar(header,'CD2_2')]
angv=[-atan(cd(1,0)/cd(0,0)),atan(cd(0,1)/cd(1,1))]*180./!pi
print,'angv = ',angv
if cd(0,0) lt 0 then ang=avg(angv) else ang=avg(angv)+180.
angr=ang*(!pi/180.)
print,[cd(0,0)/cos(angr),-cd(1,0)/sin(angr)]
print,[cd(1,1)/cos(angr),cd(0,1)/sin(angr)]
del=[avg([cd(0,0)/cos(angr),-cd(1,0)/sin(angr)]),avg([cd(1,1)/cos(angr),cd(0,1)/sin(angr)])]
print,'del = ',del

equi=sxpar(header,'equinox',count=count)
if count eq 0 then begin 
    equi=sxpar(header,'epoch',count=count)
    if count eq 0 then print,'EQUINOX keyword is not in the header!'
endif
;
RETURN
END
