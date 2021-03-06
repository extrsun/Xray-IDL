pro sisdet,rawx,rawy,ccd,sis,units,rf,xdet,ydet,dir=dir
if n_params(0) eq 0 then begin
 print,'sisdet,rawx,rawy,ccd,sis,units,rf,xdet,ydet,dir=dir'
 print,'Convert SIS raw coords to detector coords '
 print,'CCD is CCD ID and SIS = 0 or 1'
 print,'DIR = name of directory containing telescope files '
 print,'[must specify the slash in the dir name.] The filenames are hard-wired'
 print,'UNITS = 0 : output in mm, no flip or rotation'
 print,'             = 1 : output in DETX, DETY pixels, rotated only'
 print,'             = 2 : output in DETX, DETY pixels, rotated and flipped'
 print,' RF = 0 if rawx and rawy are truly RAW '
 print,' RF = 1 if rawx and rawy have been transformed with raw2new'
 print,' OUTPUT coordinates XDET and YDET '
 retall
end
if n_elements(dir) eq 0 then begin
 dir=' '
 read,'Enter name of directory containing the telescope files ',dir
endif
;check if the inputs are scalars
np = n_elements(rawx)
;get back true RAW coords if they are not truely raw already
if rf gt 0 then begin fchp2raw,rawx,rawy,xr,yr,sis,ccd
if rf eq 0 then begin
 xr=rawx & yr=rawy
endif
xr=float(xr) & yr=float(yr)
if np eq 1 then begin
 xf=fltarr(1) & yf=fltarr(1)
 xf=xr & yf=yr
 xr=xf & yr=yf
endif
xdet=fltarr(np) & ydet=fltarr(np)
xybins=1280.
if sis eq 0 then fname=dir+'sis0_telescope_def.fits' else $
fname='sis1_telescope_def.fits'
print,' reading calfile ..',fname
h=headfits(fname)
;read all the crap from the header
PLATE_X_SCALE=SXPAR(H,'FC_X_SCL')
PLATE_Y_SCALE=SXPAR(H,'FC_Y_SCL')
DET_ROTATION=SXPAR(H,'DET_ROTD')
OPTICAL_X_AXIS=SXPAR(H,'OPTAXISX')
OPTICAL_Y_AXIS=SXPAR(H,'OPTAXISY')
;FOLLOWING SHOULD BE 1.0
XYSCALE=SXPAR(H,'DET_SCAL')
DET_X_SIZE=SXPAR(H,'DET_XSIZ')
DET_X_CENTER=SXPAR(H,'DET_XCEN')
DET_X_PIX1=SXPAR(H,'DETXPIX1')
DET_X_SCALE=SXPAR(H,'DET_XSCL')
DET_Y_SIZE=SXPAR(H,'DET_YSIZ')
DET_Y_CENTER=SXPAR(H,'DET_YCEN')
DET_Y_PIX1=SXPAR(H,'DETYPIX1')
DET_Y_SCALE=SXPAR(H,'DET_YSCL')
FOV_X_SIZE=SXPAR(H,'FOV_X_MM')
FOV_Y_SIZE=SXPAR(H,'FOV_Y_MM')
;STUFF FOR 4 CCD IMAGE
NROWS=SXPAR(H,'N_ROWS')
NCOLS=SXPAR(H,'N_COLS')
ROW_WIDTH=SXPAR(H,'ROW_SIZE')
COL_WIDTH=SXPAR(H,'COL_SIZE')
RGAP=SXPAR(H,'ROW_GAP')
CGAP=SXPAR(H,'COL_GAP')
R_WIDTH=SXPAR(H,'R_WIDTH')
C_WIDTH=SXPAR(H,'C_WIDTH')
RDEAD=SXPAR(H,'R_DEAD')
CDEAD=SXPAR(H,'C_DEAD')
;NOW THE MATRIX COEFFICIENTS
c0=fltarr(6) & c1=c0 & c2=c0 & c3=c0
c0(0)=sxpar(h,'coe_x0_a')
c0(1)=sxpar(h,'coe_x0_b')
c0(2)=sxpar(h,'coe_x0_c')
c0(3)=sxpar(h,'coe_y0_a')
c0(4)=sxpar(h,'coe_y0_b')
c0(5)=sxpar(h,'coe_y0_c')
c1(0)=sxpar(h,'coe_x1_a')
c1(1)=sxpar(h,'coe_x1_b')
c1(2)=sxpar(h,'coe_x1_c')
c1(3)=sxpar(h,'coe_y1_a')
c1(4)=sxpar(h,'coe_y1_b')
c1(5)=sxpar(h,'coe_y1_c')
c2(0)=sxpar(h,'coe_x2_a')
c2(1)=sxpar(h,'coe_x2_b')
c2(2)=sxpar(h,'coe_x2_c')
c2(3)=sxpar(h,'coe_y2_a')
c2(4)=sxapr(h,'coe_y2_b')
c2(5)=sxpar(h,'coe_y2_c')
c3(0)=sxpar(h,'coe_x3_a')
c3(1)=sxpar(h,'coe_x3_b')
c3(2)=sxpar(h,'coe_x3_c')
c3(3)=sxpar(h,'coe_y3_a')
c3(4)=sxpar(h,'coe_y3_b')
c3(5)=sxpar(h,'coe_y3_c')
iseed=sxpar(h,'ran_seed')
hprint,h
;default address space?
def_scale=0
if def_scale eq 0 then begin
 x_offset=xyscale*(det_x_center - xybins)/2.
 y_offset=xyscale*(det_y_center-xybins)/2.
det_x_size=xybins*xyscale & det_y_size=xybins*xyscale
det_x_center=(det_x_size*xyscale/2.)+1.
det_y_center=(det_y_size*xyscale/2.)+1.
endif
if def_scale ne 0 then begin
 x_offset=0.0 & y_offset=0.0
 det_x_scale = det_x_scale*xyscale
 det_y_scale= det_y_scale*xyscale
 det_x_center=det_x_center*xyscale
 det_y_center=det_y_center*xyscale
endif
wc0=where((ccd eq 0),nc0)
wc1=where((ccd eq 1),nc1)
wc2=where((ccd eq 2),nc2)
wc3=where((ccd eq 3),nc3)
if nc0 gt 0 then begin
 xdet(wc0)=c0(0)+xr(wc0)*c0(1)+yr(wc0)*c0(2)
 ydet(wc0)=c0(3)+xr(wc0)*c0(4)+yr(wc0)*c0(5)
endif
if nc1 gt 0 then begin
 xdet(wc1)=c0(0)+xr(wc1)*c0(1)+yr(wc1)*c0(2)
 ydet(wc1)=c0(3)+xr(wc1)*c0(4)+yr(wc1)*c0(5)
endif
if nc2 gt 0 then begin
 xdet(wc2)=c0(0)+xr(wc2)*c0(1)+yr(wc2)*c0(2)
 ydet(wc2)=c0(3)+xr(wc2)*c0(4)+yr(wc2)*c0(5)
endif
if nc3 gt 0 then begin
 xdet(wc3)=c0(0)+xr(wc3)*c0(1)+yr(wc3)*c0(2)
 ydet(wc3)=c0(3)+xr(wc3)*c0(4)+yr(wc3)*c0(5)
endif
xdet=xdet*xyscale-det_x_center
ydet=ydet*xyscale-det_y_center
print,'done the main transformation'
if units eq 0 then begin
;request was in mm, no flip or rotation
 xdet=det_x_scale*xdet & ydet=det_y_scale*ydet
 print,' Remember you requested xdet and ydet in MM with no flip or rotation '
 goto, fin2
endif
if units ge 1 and units le 2 then begin
 deta=det_rotation*!pi/180.
 sina=sin(deta) & cosa=cos(deta)
 rotx=xdet*cosa-ydet*sina
 roty=xdet*sina+ydet*cosa
 print,' Remember you requested XDET YDET in pixels with rotation only'
endif
if units eq 1 then goto, fin
if units eq 2 then begin
 xflip=1.0 & yflip=-1.0
 rotx=rotx*xflip+det_x_center
 roty=roty*yflip+det_x_center
 print,' Remember you requested XDET, YDET in pixels with rotation+flip'
endif
fin: xdet=rotx & ydet=roty
xdet=fix(xdet)+1 & ydet=fix(ydet)+1
xdet=xdet*(xdet ge detxpix1) & xdet=xdet*(xdet le det_x_size)
ydet=ydet*(ydet ge detypix1) & ydet=ydet*(ydet le det_y_size)
print,' ** WARNING ** these pixel units start from 1'
print,' subtract 1 for IDL images'
fin2: print,' '
end
