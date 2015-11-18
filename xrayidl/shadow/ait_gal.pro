pro ait_gal,imait,xlon,xlat,sel=sel,choice=choice,pix=pix
;REVERSE FORMULA; XLON0 IS THE CENTER LONGITUDE OF THE MAP.
;     ARC-SINE AND ARC-COSINE FUNCTIONS NEEDED.
sz=size(imait)
xsize=sz(1)
ysize=sz(2)
cpx=(xsize-1.)*0.5
cpy=(ysize-1.)*0.5
xlon0=0.
 R2D = 45. / ATAN(1.)
 if n_elements(pix) eq 0 then PIX = 1. ; pixels per degree
dr=(!pi/180.)
if n_elements(choice) eq 0 then choice = 0
if choice eq 1 then begin
   xlat=xlat*dr
   xlon=xlon*dr
   RHO = ACOS( COS(XLAT) * COS((XLON-XLON0)/2.) )
   THETA = ASIN( COS(XLAT) * SIN((XLON-XLON0)/2.) / SIN(RHO) )
   F = 2. * PIX * R2D * SIN(RHO/2.)
   SAMPLE = -2. * F * SIN(THETA)
   XLINE = -F * COS(THETA)
   IF(XLAT LT 0.) then  XLINE = -XLINE
   xline=xline+cpy
   sample=sample+cpx
   sel=long(sample+0.5)*xsize+long(xline+0.5)
endif else begin
  num=xsize*ysize
  loc=lindgen(num)
  if n_elements(sel) ne 0 then loc=loc(sel)
  xx=loc mod xsize
  yy=loc/xsize
  xline=cpy-yy ;y axis
  sample=xx - cpx ;x axis
  Y = -XLINE / (PIX * 2. * R2D)
  X = -SAMPLE / (PIX * 2. * R2D)
  A = SQRT(4.-X*X-4.*Y*Y)
  XLAT = R2D * ASIN(A*Y)
  XLON = XLON0 + 2. * R2D * ASIN(A*X/(2.*COS(XLAT/R2D)))
  if xlon0 eq 0. then begin ;anti-center direction restored to 180 deg
	ss=where(xlon eq 0. and abs(x) ne 0.,nss) 
	if nss ne 0 then xlon(ss)=180.
  endif
endelse
return
end
