function multicolorimg, image, rbrightness=rbrightness, fscale=fscale, coeffindex=coeffindex, logmean=logmean, scalezoom=scalezoom, bscalerange=bscalerange, colortype=colortype, coefficient=coefficient

cnumber=n_elements(image[*,0,0])
xnumber=n_elements(image[0,*,0])
ynumber=n_elements(image[0,0,*])
multic_img=fltarr(3,xnumber,ynumber)
scaleimg=fltarr(xnumber,ynumber)

if colortype eq 0 then begin
for j=0, cnumber-1 do begin
	if strcmp(fscale[j],'alog',4) then begin
		scaleimg[*,*]=CALL_FUNCTION(fscale[j], image[j,*,*]+10.0)
		image[j,*,*]=normalizeimg(scaleimg)
	endif else begin
		scaleimg[*,*]=CALL_FUNCTION(fscale[j], image[j,*,*])
		image[j,*,*]=normalizeimg(scaleimg)
	endelse
endfor
endif

if colortype eq 1 then begin
for j=0, cnumber-1 do begin
	amv=logmean[j]*bscalerange[j]
	scaleimg[*,*]=bscale(CALL_FUNCTION(fscale[j],(image[j,*,*]-amv)),CALL_FUNCTION(fscale[j],(logmean[j]-amv)),CALL_FUNCTION(fscale[j],(logmean[j]-amv)*scalezoom[j]))
print,'mean= ',logmean[j]-amv,' max= ',(logmean[j]-amv)*scalezoom[j],' image value= ',image[j,100,100]
	image[j,*,*]=normalizeimg(scaleimg)
endfor
endif

if keyword_set(coefficient) eq 0 then begin

	coefficient=fltarr(3,cnumber)

	for k=1, cnumber do begin
		if k le float(cnumber+1)/2. then begin
			coefficient[0,k-1]=float(1.-2.*(k-1.)/(cnumber-1.))
			coefficient[1,k-1]=float(2.*(k-1.)/(cnumber-1.))
			coefficient[2,k-1]=0.
		endif else begin
			coefficient[0,k-1]=0.
			coefficient[1,k-1]=float(2.*(cnumber-k)/(cnumber-1.))
			coefficient[2,k-1]=float(2.*(k-1.)/(cnumber-1.)-1.)
		endelse
	endfor

	coefficient=coefficient^coeffindex
	for i=0,cnumber-1 do begin
		coefficient[*,i]=coefficient[*,i]/(coefficient[0,i]+coefficient[1,i]+coefficient[2,i])
	endfor

endif 

multic_img[*,*,*]=0.

for k=1, cnumber do begin
	multic_img[0,*,*]=multic_img[0,*,*]+coefficient[0,k-1]*image[k-1,*,*]*rbrightness[k-1]
	multic_img[1,*,*]=multic_img[1,*,*]+coefficient[1,k-1]*image[k-1,*,*]*rbrightness[k-1]
	multic_img[2,*,*]=multic_img[2,*,*]+coefficient[2,k-1]*image[k-1,*,*]*rbrightness[k-1]
endfor

return, {IMG:multic_img, COEF:coefficient}

end 


function normalizeimg, image

minvalue=min(image)
maxvalue=max(image)
return, float((image[*,*]-minvalue)/(maxvalue-minvalue))

end


pro drawmulticolorimg, imagefile, fitsheadfile=fitsheadfile, description=description, psfile=psfile, jpegfile=jpegfile, cor=cor, figtitle=figtitle, colorbarwidth=colorbarwidth, colorbarlength=colorbarlength, colorbarbrightness=colorbarbrightness, nocolorbar=nocolorbar, windowplot=windowplot, xcor=xcor, ycor=ycor, yframeratio=yframeratio, charsizoom=charsizoom, charmax=charmax, thickzoom=thickzoom, xtitleposi=xtitleposi, ytitleposi=ytitleposi, titlesize=titlesize, rbrightness=rbrightness, fscale=fscale, coeffindex=coeffindex, meanfactor=meanfactor, logmean=logmean, scalezoom=scalezoom, bscalerange=bscalerange, colortype=colortype, otherwords=otherwords, otherposix=otherposix, otherposiy=otherposiy, othersize=othersize, otherthick=otherthick, noframe=noframe, coefficient=coefficient, ps_size=ps_size
;+
; NAME:
;		drawmulticolorimg
;
; PURPOSE:
;		draw a multi-color image
;
; CALLING SEQUENCE:
;		drawmulticolorimg(imagefile, [fitsheadfile=, description=, psfile=, jpegfile=, cor=, figtitle=, colorbarwidth=, colorbarlength=, colorbarbrightness=, nocolorbar=, windowplot=, xcor=, ycor=, yframeratio=, charsizoom=, charmax=, thickzoom=, xtitleposi=, ytitleposi=, titlesize=, rbrightness=, fscale=, coeffindex=, meanfactor=, logmean=, scalezoom=, bscalerange=, colortype=, otherwords=, otherposix=, otherposiy=, othersize=, otherthick=, noframe=])   
;
; INPUTS:
;		imagefile = input fits file names, with headers contain necessary informations.
;
; OUTPUTS:
;		a ps file named psfile or a jpeg file named jpegfile.
;
; OPTIONAL INPUTS:
;		fitsheadfile = fits header used to cast other images, if not set, the header of the first fits file in imagefile will be used.
;		description = short description of the multi-band data, will be marked beside the color bar.
;		cor = the normlized coordinates of the image (def: cor=[0.,1.,0.,1.]).
;		figtitle = if set, a title of the figure will be added.
;		colorbarwidth, colorbarlength, colorbarbrightness = width, length and brightness of the color bar (def: width=0.02, length=1.0, brightness=1.5).
;		nocolorbar = if set, no color bar will be ploted.
;		windowplot = if set, multi-band images will be ploted seperately in different windows for comparison.
;		xcor, ycor = left bottom corner of the color bar, NOTE that these should not be set to 0 (def: xcor=0.2, ycor=0.088).
;		yframeratio = to adjust the relative position of the description to the colorbar, it should be set to 1.0 when drawing without frame (def: yframeratio=0.75).
;		charsizoom, thickzoom = to adjust the character size and thickness of the description (def: charsizoom=3.5, thickzoom=5.0).
;		charmax = max character size of the description (def: charmax=1.5).
;		xtitleposi, ytitleposi = x, y position of the title (def: xtitleposi=0.45, ytitleposi=0.9).
;		titlesize = size of the title (def: titlesize=2.5).
;		rbrightness = relative brightness of different images (def: rbrightness[*]=1.0).
;		fscale = scale of different images, NOTE that you should use idl functions here like 'alog10' (def: fscale[*]='abs', after normalization, means linear).
;		coeffindex = to adjust the color table (def: coeffindex=0.25).
;		meanfactor, logmean, scalezoom, bscalerange = adjust the image color used in colortype 1 (def: meanfactor[*]=1.0, logmean[*]=mean(img), scalezoom[*]=1000.0, bscalerange=0.9).
;		colortype = type of color table, colortype should be 1 or 0 (def: colortype=0).
;		otherwords = some other words to write on the image, like the label of the image (a).
;		otherposix, otherposiy, othersize, otherthick = x, y position, size and thick of otherwords.
;		noframe = if set, no frame will be ploted.
;
; NOTES:
;		(1) wqd's package xidl is needed.
;		(2) if there are more than one 'CROTA*' parameter which is lt 360 and gt 0, the procedure will be stopped.
;
; REVISION HISTORY:
;		written by ljt, Mar. 28, 2007, in ljtastro, procedure version 1.0
;
; EXAMPLES:
;		fitsheadfile='/home/ljt/ljt/data/AboutAGN/NGC5775/Chandra/2940/image/smoothimg3/as_fton5_s_subback.fits'
;		imagefile=strarr(6)
;		imagefile[0]='/home/ljt/ljt/data/AboutAGN/NGC5775/Chandra/2940/mdata/fromIrwin/N5775HI(UN).FITS'
;		imagefile[1]='/home/ljt/ljt/data/AboutAGN/NGC5775/Chandra/2940/mdata/fromIrwin/N5775_20CM_CONT_HIGH_ROT.FITS'
;		imagefile[2]='/home/ljt/ljt/data/AboutAGN/NGC5775/Chandra/2940/mdata/r10994176/ch4/pbcd/SPITZER_I4_10994176_0000_4_E2022344_maic.fits'
;		imagefile[3]='/home/ljt/ljt/data/AboutAGN/NGC5775/Chandra/2940/mdata/fromIrwin/N5775_HA_COLLINS.FITS'
;		imagefile[4]='/home/ljt/ljt/data/AboutAGN/NGC5775/Chandra/2940/image/smoothimg3/as_fton5_s_subback.fits'
;		imagefile[5]='/home/ljt/ljt/data/AboutAGN/NGC5775/Chandra/2940/image/smoothimg3/as_fton5_h_subback.fits'
;		description=['H I','20 cm','8!7l!3m','H!7a!3','0.3-1.5 keV','1.5-7 keV']
;		psfile='/home/ljt/ljt/data/AboutAGN/NGC5775/Chandra/NGC5775.ps'
;		title='!7NGC 5775'
;		rbrightness=[1.0,1.5,1.0,1.0,1.5,0.6]
;		fscale=['abs','abs','abs','abs','alog10','alog10']
;		meanfactor=[1.0,1.0,1.0,1.0,0.7,1.0]
;		drawmulticolorimg,imagefile,fitsheadfile=fitsheadfile,description=description,psfile=psfile,figtitle=title,fscale=fscale, colortype=1, rbrightness=rbrightness, meanfactor=meanfactor
;


if keyword_set(fitsheadfile) eq 0 then begin
	fitsheadfile=imagefile[0]
endif
if keyword_set(cor) eq 0 then begin
	cor=[0.,1.,0.,1.] 
endif
if keyword_set(colorbarbrightness) eq 0 then begin
	colorbarbrightness=1.5
endif
if keyword_set(titlesize) eq 0 then begin
	titlesize=2.5
endif
if keyword_set(xtitleposi) eq 0 then begin
	xtitleposi=0.45
endif
if keyword_set(ytitleposi) eq 0 then begin
	ytitleposi=0.9
endif
if keyword_set(colorbarwidth) eq 0 then begin
	colorbarwidth=0.02
endif
if keyword_set(colorbarlength) eq 0 then begin
	colorbarlength=1.0
endif
if keyword_set(xcor) eq 0 then begin
	xcor=0.2
endif 
if keyword_set(ycor) eq 0 then begin
	ycor=0.088
endif
if keyword_set(yframeratio) eq 0 then begin
	yframeratio=0.75
endif
if keyword_set(charsizoom) eq 0 then begin
	charsizoom=3.5
endif
if keyword_set(charmax) eq 0 then begin
	charmax=1.5
endif
if keyword_set(thickzoom) eq 0 then begin
	thickzoom=5.0
endif
if keyword_set(coeffindex) eq 0 then begin
	coeffindex=0.25
endif
if keyword_set(bscalerange) eq 0 then begin
	bscalerange=0.9
endif
if keyword_set(colortype) eq 0 then begin
	colortype=0
endif

cnumber=n_elements(imagefile)
forfitshead=mrdfits(fitsheadfile,0,casthead)
xnumber=n_elements(forfitshead[*,0])
ynumber=n_elements(forfitshead[0,*])
image=fltarr(cnumber,xnumber,ynumber)
trimage=fltarr(xnumber,ynumber,3)
inroll=fltarr(cnumber)

if keyword_set(scalezoom) eq 0 then begin
	scalezoom=fltarr(cnumber)
	scalezoom[*]=1000.0
endif
if keyword_set(rbrightness) eq 0 then begin
	rbrightness=fltarr(cnumber)
	rbrightness[*]=1.0
endif
if keyword_set(fscale) eq 0 then begin
	fscale=strarr(cnumber)
	fscale[*]='abs'
endif
if keyword_set(ps_size) eq 0 then begin
	ps_size=[float(xnumber)/50.,float(ynumber)/50.]
endif


for i=0,cnumber-1 do begin
	forfitshead=mrdfits(imagefile[i],0,fitshead)
	inrollinhead=sxpar(fitshead,'CROTA*')
	if where(abs(inrollinhead) gt 0 and abs(inrollinhead) lt 360) eq -1 then begin
		inrollnumber=0
	endif else begin
		inrollnumber=n_elements(where(abs(inrollinhead) gt 0 and abs(inrollinhead) lt 360))
	endelse
	if inrollnumber eq 0 then begin
		inroll[i]=0.0
	endif
	if inrollnumber eq 1 then begin
		inroll[i]=inrollinhead[where(abs(inrollinhead) gt 0 and abs(inrollinhead) lt 360)]
	endif
	if inrollnumber gt 1 then begin
		print,'more than one inroll number'
		goto,OUTSTEP
	endif
endfor

for i=0,cnumber-1 do begin
	cast,imagefile[i],casthead,outa=castimg,inroll=inroll[i]
;	normalimage=normalizeimg(castimg)
;	image[i,*,*]=normalimage[*,*]
	image[i,*,*]=castimg[*,*]
endfor

if keyword_set(meanfactor) eq 0 then begin
	meanfactor=fltarr(cnumber)
	meanfactor[*]=1.0
endif

if keyword_set(logmean) eq 0 then begin
logmean=fltarr(cnumber)
for i=0,cnumber-1 do begin
	logmean[i]=mean(image[i,*,*])*meanfactor[i]
endfor
endif

if keyword_set(coefficient) eq 0 then begin
	data=multicolorimg(image, rbrightness=rbrightness, fscale=fscale, coeffindex=coeffindex, logmean=logmean, scalezoom=scalezoom, bscalerange=bscalerange, colortype=colortype)
endif else begin
	data=multicolorimg(image, rbrightness=rbrightness, fscale=fscale, coeffindex=coeffindex, logmean=logmean, scalezoom=scalezoom, bscalerange=bscalerange, colortype=colortype,coefficient=coefficient)
endelse
multiimage=data.IMG
coefficient=data.COEF
trimage[*,*,0]=multiimage[0,*,*]
trimage[*,*,1]=multiimage[1,*,*]
trimage[*,*,2]=multiimage[2,*,*]

if keyword_set(nocolorbar) eq 0 then begin
	xlength=fix(xnumber*colorbarwidth)
	ylength=fix(ynumber/cnumber*colorbarlength)
	for i=0,5 do begin
		trimage[0:xlength-1,i*ylength:(i+1)*ylength-1,0]=coefficient[0,i]*colorbarbrightness
		trimage[0:xlength-1,i*ylength:(i+1)*ylength-1,1]=coefficient[1,i]*colorbarbrightness
		trimage[0:xlength-1,i*ylength:(i+1)*ylength-1,2]=coefficient[2,i]*colorbarbrightness
		trimage[xlength,i*ylength:(i+1)*ylength-1,0]=1.0
		trimage[xlength,i*ylength:(i+1)*ylength-1,1]=1.0
		trimage[xlength,i*ylength:(i+1)*ylength-1,2]=1.0
	endfor
endif

set_plot,'ps'
device,Filename=psfile,bits=8,color=1,xsize=ps_size[0],ysize=ps_size[1],yoffset=26,xoffset=2

if keyword_set(noframe) eq 0 then begin
	cont_grey,trimage,casthead,cor=cor,true=3,mr=0.02,/nocon,/def,/ps,f_c=0,barf=0.
endif else begin	
	cor=[0.,1.,0.,1.] 
	cont_grey,trimage,casthead,true=3,mr=0,/nocon,/ps,/notick,barfactor=0,cor=cor
endelse

if keyword_set(description) ne 0 then begin
	tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
	!color=255
	for i=0,5 do begin
		xyouts, xcor+colorbarwidth*1.5, ycor+(i+0.5)*colorbarlength*yframeratio/float(cnumber), description[i], /norm, charsi=(charsizoom*colorbarlength<charmax), CHARTHICK=thickzoom*colorbarlength
	endfor
endif

if keyword_set(figtitle) ne 0 then begin
	!color=0
	xyouts,xtitleposi,ytitleposi,figtitle,/norm,charsi=titlesize,CHARTHICK=10,Font=0
endif

if keyword_set(otherwords) ne 0 then begin
	!color=255
	xyouts,otherposix,otherposiy,otherwords,/norm,charsi=othersize,CHARTHICK=otherthick,Font=0
endif

device,/close
set_plot,'x'

if keyword_set(jpegfile) ne 0 then begin
	window,0,xsize=xnumber,ysize=ynumber
	cont_grey,trimage,casthead,cor=cor,true=3,mr=0.02,/nocon,/def,/full,f_c=0,barf=0.
	s=tvrd(0,0,true=3)
	write_jpeg,jpegfile,s,true=3,qua=100
endif

if keyword_set(windowplot) ne 0 then begin
	for i=0,cnumber-1 do begin
		window,i+1,xsize=xnumber,ysize=ynumber
		trimage[*,*,0]=coefficient[0,0]*image[i,*,*]
		trimage[*,*,1]=coefficient[1,0]*image[i,*,*]
		trimage[*,*,2]=coefficient[2,0]*image[i,*,*]
		cont_grey,trimage,casthead,cor=cor,true=3,mr=0,/ps,/nocon,/full,f_c=-1,barf=0.
	endfor
endif

print,coefficient

OUTSTEP:
end
