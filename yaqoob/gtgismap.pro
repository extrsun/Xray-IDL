pro gtgismap,postyp,gistyp,dir,gmap,cal,detx,dety,deltax,deltay,coeff
if n_params(0) eq 0 then begin
  print,' GTGISMAP, postyp, gistyp, dir, gmap,cal,detx,dety,deltax,deltay,coeff '
  print,' Get GIS gain map from calibration file '
  print,' POS DET METHOD = FLF (postyp=0); GIS2: g2_teldef_flf_030893.fits '
  print,' POS DET METHOD = FLF (postyp=0); GIS3: g3_teldef_flf_030893.fits '
  print,' POS DET METHOD =POW2 (postyp=1); GIS2: gis2_telescope_def_cu_pow2.fits '
  print,' POS DET METHOD =POW2 (postyp=1); GIS3: gis3_telescope_def_cu_pow2.fits '
  print,' calratio is the gain norm at pos calx0 caly0 '
retall
end
;if postyp eq 0 and gistyp eq 2 then fname=dir+'g2_teldef_flf_030893.fits'
;if postyp eq 0 and gistyp eq 3 then fname=dir+'g3_teldef_flf_030893.fits'
;if postyp eq 0 and gistyp eq 2 then fname=dir+'gis2_ano_on_flf_200494.fits'
;if postyp eq 0 and gistyp eq 3 then fname=dir+'gis3_ano_on_flf_200494.fits'
if postyp eq 0 and gistyp eq 2 then fname=dir+'gis2_ano_on_flf_180295.fits'
if postyp eq 0 and gistyp eq 3 then fname=dir+'gis3_ano_on_flf_180295.fits'
;below is the file made for G3 from inflight data
;if postyp eq 0 and gistyp eq 3 then fname=dir+'gis3_phnew_tbl_flf_200494.fits'
if postyp eq 1 and gistyp eq 2 then fname=dir+'gis2_ano_on_pow2_200494.fits'
if postyp eq 1 and gistyp eq 3 then fname=dir+'gis3_ano_on_pow2_200494.fits'
h=headfits(fname) 
print,' ** USING GIS GAIN MAP FILE ',fname
;get some crucial header keywords
detx={xdet,xyscale:0.0,size:0.0,center:0.0,scale:0.0,offset:0.0,pix1:0}
dety={ydet,xyscale:0.0,size:0.0,center:0.0,scale:0.0,offset:0.0,pix1:0}
detx.xyscale=sxpar(h,'DET_SCAL') & dety.xyscale=detx.xyscale
;actually the above xyscale shouls be GIS_SIZE/RAWXBINS - i.e 256/256
;at present DET_SCAL is 1.0 so watch out
detx.size=sxpar(h,'DET_XSIZ')
DETX.CENTER=SXPAR(H,'DET_XCEN')
DETX.SCALE=SXPAR(H,'DET_XSCL')
DETX.OFFSET=DETX.CENTER*DETX.SCALE
detx.pix1=sxpar(h,'detxpix1')
DETY.SIZE=SXPAR(H,'DET_YSIZ')
DETY.CENTER=SXPAR(H,'DET_YCEN')
DETY.SCALE=SXPAR(H,'DET_YSCL')
DETY.OFFSET=DETY.CENTER*DETY.SCALE
dety.pix1=sxpar(h,'detypix1')
det_rot=sxpar(h,'DET_ROTD')*!pi/180.
if postyp eq 0 then pmeth = 1
if postyp gt 0 then pmeth = 0
coeff=fltarr(6)
if pmeth eq 0 then begin
COEFF(0)=SXPAR(H,'COE_X1_A')
COEFF(1)=SXPAR(H,'COE_X1_B')
COEFF(2)=SXPAR(H,'COE_X1_C')
COEFF(3)=SXPAR(H,'COE_Y1_A')
COEFF(4)=SXPAR(H,'COE_Y1_B')
COEFF(5)=SXPAR(H,'COE_Y1_C')
endif
if pmeth eq 1 then begin
COEFF(0)=SXPAR(H,'COE_X2_A')
COEFF(1)=SXPAR(H,'COE_X2_B')
COEFF(2)=SXPAR(H,'COE_X2_C')
COEFF(3)=SXPAR(H,'COE_Y2_A')
COEFF(4)=SXPAR(H,'COE_Y2_B')
COEFF(5)=SXPAR(H,'COE_Y2_C')
endif
gmap=readfits(fname,h2,ext=2)
deltax=readfits(fname,h4,ext=4)
deltay=readfits(fname,h6,ext=6)
cal={calib,ratio:0.0,x0:0.0,y0:0.0,detcos:0.0,detsin:0.0,xyoffset:0.0 $
,adu:0.0,norm:0.0}
cal.ratio=sxpar(h,'CALRATIO')
cal.x0=sxpar(h,'CAL_XO') & cal.y0=sxpar(h,'CAL_YO')
cal.detcos=cos(det_rot) & cal.detsin=sin(det_rot)
cal.xyoffset=0.5*detx.xyscale
cal.adu=sxpar(h,'CAL_ADU')
cal.norm=sxpar(h,'CAL_NORM')
return
end
