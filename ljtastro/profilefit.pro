; $Id: profilefit.pro,v 1.1 2009/01/06
;
; Copyright (c), Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.
;===============================================================
; NOTE:
;	If you want to add your own functions, please keep the function name as 'functionname' while the fit procedure name as 'functionname'+'fit'.
; Available function list:
;	fgauss (4 parameters): 			f(x)=A0*exp(-(x-A1)^2/A2^2/2)+A3
;	f2gauss (7 parameters):			f(x)=A0*exp(-(x-A1)^2/A2^2/2)+A3*exp(-(x-A4)^2/A5^2/2)+A6
;	fmexp (4 parameters):			f(x)=A0*exp(-abs(x-A1)/A2)+A3
;	fconstexp (5 parameters):		When x<0: f(x)=A0*exp(x/A1)+A4
;						When x>=0: f(x)=A2*exp(-x/A3)+A4
;	fnobckconstIexp (3 parameters):		When x<0: f(x)=A0*exp(x/A1)
;						When x>=0: f(x)=A0*exp(-x/A2)
;	fexpn (3 parameters):			f(x)=A0*x^A1+A2
;	fKband (3 parameters):			f(x)=A0*Kprofile^A1+A2 (NOTE: the Kprofile is from a file in the current directory named as 'n'+number)
;						(NOTE: the fitting of A2 seems problematic, so better to fix it.)
;
;===============================================================
; HISTORY:
; Modified by ljt 2009/01/06: add fexpn, fexpnm and fKband function.
; Modified by ljt 2009/01/06: delete some useless functions, add FITA keyword, the version is updated to v1.1.
;===============================================================
;Gauss fit (4 parameters)
;f(x)=A0*exp(-(x-A1)^2/A2^2/2)+A3

FUNCTION fgauss,X,A
  bx1=A[0]*EXP(-(X-A[1])^2/(A[2])^2/2.)
  F=bx1+A[3]
return,F
END

;------------------------------------------------------------

PRO fgaussfit, X, A, F, pder

  bx1=A[0]*EXP(-(X-A[1])^2/(A[2])^2/2.)
  F=bx1+A[3]

  pder=fltarr(n_elements(X),4)
  pder[*,0]=bx1/A[0]
  pder[*,1]=bx1*(X-A[1])/A[2]
  pder[*,2]=bx1*(X-A[1])^2/(A[2])^3
  pder[*,3]=replicate(1.0, N_ELEMENTS(X))

END

;===============================================================
;Double gauss fit (7 parameters)
;f(x)=A0*exp(-(x-A1)^2/A2^2/2)+A3*exp(-(x-A4)^2/A5^2/2)+A6

FUNCTION f2gauss,X,A
  bx1=A[0]*EXP(-(X-A[1])^2/(A[2])^2/2.)
  bx2=A[3]*EXP(-(X-A[4])^2/(A[5])^2/2.)
  F=bx1+bx2+A[6]
return,F
END

;------------------------------------------------------------

PRO f2gaussfit, X, A, F, pder
;tmp setting
A[1]=0.0
  bx1=A[0]*EXP(-(X-A[1])^2/(A[2])^2/2.)
  bx2=A[3]*EXP(-(X-A[4])^2/(A[5])^2/2.)
  F=bx1+bx2+A[6]

  pder=fltarr(n_elements(X),7)
  pder[*,0]=bx1/A[0]
  pder[*,1]=bx1*(X-A[1])/A[2]
  pder[*,2]=bx1*(X-A[1])^2/(A[2])^3
  pder[*,3]=bx2/A[3]
  pder[*,4]=bx2*(X-A[4])/A[5]
  pder[*,5]=bx2*(X-A[4])^2/(A[5])^3
  pder[*,6]=replicate(1.0, N_ELEMENTS(X))
;tmp setting
pder[*,1]=X-X

END

;===============================================================
;Exponential fit (4 parameters)
;f(x)=A0*exp(-abs(x-A1)/A2)+A3

FUNCTION fmexp,X,A
  Ig=A[0]
  z0=A[1]
  zh=A[2]
  F=Ig*exp(-abs(X-z0)/zh)+A[3]
return,F
END

;------------------------------------------------------------

PRO fmexpfit, X, A, F, pder

  Ig=A[0]
  z0=A[1]
  zh=A[2]
  F=Ig*exp(-abs(X-z0)/zh)+A[3]

  pder=fltarr(n_elements(X),4)
  pder[*,0]=exp(-abs(X-z0)/zh)
  pder[*,1]=(X-z0)/abs(X-z0)*Ig/zh*exp(-abs(X-z0)/zh)
  pder[*,2]=abs(Ig*(X-z0)/zh^2)*exp(-abs(X-z0)/zh)
  pder[*,3]=replicate(1.0, N_ELEMENTS(X))

END

;===============================================================
;Exponential fit for both the negative and positive sides of the vertical profile with constant background level (5 parameters)
;For negative x: f(x)=A0*exp(x/A1)+A4
;For positive x: f(x)=A2*exp(-x/A3)+A4

FUNCTION fconstexp,X,A
  I1=A[0]
  z1=A[1]
  I2=A[2]
  z2=A[3]
  const=A[4]
  F=fltarr(n_elements(X))
 for i=0,n_elements(X)-1 do begin
  if X[i] lt 0 then begin
   F[i]=I1*exp(X[i]/z1)+const
  endif
  if X[i] ge 0 then begin
   F[i]=I2*exp(-X[i]/z2)+const
  endif
 endfor
return,F
END

;------------------------------------------------------------

PRO fconstexpfit, X, A, F, pder

  I1=A[0]
  z1=A[1]
  I2=A[2]
  z2=A[3]
  const=A[4]
  F=fltarr(n_elements(X))
  pder=fltarr(n_elements(X),n_elements(A))
 for i=0, n_elements(X)-1 do begin
  if X[i] lt 0 then begin
   F[i]=I1*exp(X[i]/z1)+const
  endif
  if X[i] ge 0 then begin
   F[i]=I2*exp(-X[i]/z2)+const
  endif
  if X[i] lt 0 then begin
   pder[i,0]=exp(X[i]/z1)
   pder[i,1]=-I1*X[i]/z1^2*exp(X[i]/z1)
   pder[i,2]=0.
   pder[i,3]=0.
  endif
  if X[i] ge 0 then begin
   pder[i,0]=0.
   pder[i,1]=0.
   pder[i,2]=exp(-X[i]/z2)
   pder[i,3]=I2*X[i]/z2^2*exp(-X[i]/z2)
  endif
 endfor
   pder[*,4]=replicate(1.0, N_ELEMENTS(X))
END

;===============================================================
;Exponential fit for both the negative and positive sides of the vertical profile with constant normalization and 0 background level (3 parameters)
;For negative x: f(x)=A0*exp(x/A1)
;For positive x: f(x)=A0*exp(-x/A2)

FUNCTION fnobckconstIexp,X,A
  It=A[0]
  z1=A[1]
  z2=A[2]
  F=fltarr(n_elements(X))
 for i=0,n_elements(X)-1 do begin
  if X[i] lt 0 then begin
   F[i]=It*exp(X[i]/z1)
  endif
  if X[i] ge 0 then begin
   F[i]=It*exp(-X[i]/z2)
  endif
 endfor
return,F
END

;------------------------------------------------------------

PRO fnobckconstIexpfit, X, A, F, pder

  It=A[0]
  z1=A[1]
  z2=A[2]
  F=fltarr(n_elements(X))
  pder=fltarr(n_elements(X),n_elements(A))
 for i=0, n_elements(X)-1 do begin
  if X[i] lt 0 then begin
   F[i]=It*exp(X[i]/z1)
  endif
  if X[i] ge 0 then begin
   F[i]=It*exp(-X[i]/z2)
  endif
  if X[i] lt 0 then begin
   pder[i,0]=exp(X[i]/z1)
   pder[i,1]=-It*X[i]/z1^2*exp(X[i]/z1)
   pder[i,2]=0.
  endif
  if X[i] ge 0 then begin
   pder[i,0]=exp(-X[i]/z2)
   pder[i,1]=0.
   pder[i,2]=It*X[i]/z2^2*exp(-X[i]/z2)
  endif
 endfor

END

;===============================================================
;Exponential fit with index as parameter (3 parameters)
;f(x)=A0*x^A1+A2

FUNCTION fexpn,X,A
  F=A[0]*X^A[1]+A[2]
return,F
END

;------------------------------------------------------------

PRO fexpnfit, X, A, F, pder

  F=A[0]*X^A[1]+A[2]

  pder=fltarr(n_elements(X),3)
  pder[*,0]=X^A[1]
  pder[*,1]=A[0]*alog(X)*X^A[1]
  pder[*,2]=replicate(1.0, N_ELEMENTS(X))

END

;===============================================================
;for vertical or parallel profiles.

FUNCTION Kfun,Kprofile,Xlow,Xhigh,number
 data=fltarr(3,number)
 dataadd=fltarr(number,5)
 openr,lun,'n'+strtrim(string(fix(Kprofile)),2),/get_lun
  readf,lun,dataadd
 close,lun
 free_lun,lun
 data[0,*]=dataadd[*,0]
 data[1,*]=dataadd[*,3]
 data[2,*]=dataadd[*,4]
 subscript=where((data[0,*] ge Xlow) and (data[0,*] le Xhigh),totalnumber)
 if totalnumber gt 0 then begin
  return,total(data[1,subscript])/float(totalnumber)
 endif else begin
  return,0
 endelse
END

;------------------------------------------------------------

FUNCTION fKband,X,A
;maxwidth is the maximum width to average the K band intensity in X direction for each data point; Kprofile is the K band luminosity profile; Knumber is the number of data points in Kprofile.
;Xl-Xb=norm*K^alpha
  norm=A[0]
  alpha=A[1]
  Xb=A[2]
  Kprofile=A[3]
  Knumber=A[4]
  maxwidth=A[5]
  F=fltarr(n_elements(X))
 for i=0,n_elements(X)-1 do begin
  if i gt 0 and i lt n_elements(X)-1 then begin
   xwidth=(maxwidth<(abs(X[i+1]-X[i-1])/2.))
  endif else begin
   xwidth=maxwidth
  endelse
  F[i]=norm*(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber))^alpha+Xb
 endfor
return,F
END

;------------------------------------------------------------

PRO fKbandfit, X, A, F, pder
;Xl=norm*K^alpha+Xb
;Parameters to be fitted:
;A[0]=norm
;A[1]=alpha
;A[2]=Xb
;Fixed parameters:
;A[3]=Kprofile
;A[4]=Knumber
;A[5]=maxwidth

  norm=A[0]
  alpha=A[1]
  
  Xb=A[2]
  Kprofile=A[3]
  Knumber=A[4]
  maxwidth=A[5]

  F=fltarr(n_elements(X))
  pder=fltarr(n_elements(X),n_elements(A))
 for i=0,n_elements(X)-1 do begin
  if i gt 0 and i lt n_elements(X)-1 then begin
   xwidth=(maxwidth<(abs(X[i+1]-X[i-1])/2.))
  endif else begin
   xwidth=maxwidth
  endelse
  F[i]=norm*(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber))^alpha+Xb
  pder[i,0]=(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber))^alpha
  pder[i,1]=norm*alog(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber))*(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber))^alpha
 endfor
  pder[*,2]=replicate(0.0, N_ELEMENTS(X))

END

;===============================================================

pro profilefit,X,Y,err=err,A=A,funname=funname,weights=weights,xtitle=xtitle,ytitle=ytitle,pnum=pnum,xrange=xrange,yrange=yrange,FITA=FITA

;===============================================================
;+
; NAME:
;       profilefit
; PURPOSE:
;       calculate the vertical or parallel profile.
; CALLING SEQUENCE:
;  	profilefit,X,Y,err=err,A=A,funname=funname,weights=weights,xtitle=xtitle,ytitle=ytitle,pnum=pnum,xrange=xrange,yrange=yrange,FITA=FITA
; INPUT PARAMETERS:
;	X: x value array.
;	Y: y value array.
;	A: initial value of parameter array of the fitting function, refer to the function defination above.
;	funname: name of the fitting function.
; OPTIONAL INPUT PARAMETERS:
;	err: y error value array, if not set, the default y error is 0.
;	weights: weights of different points, the default value is 1.0/float(n_elements(Y)).
;	xtitle: title of x axis for plot, the default value is 'Minor-axis Distance (arcmin)'.
;	ytitle: title of y axis for plot, the default value is '!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'.
;	pnum: number of the points for plotting the fitted model, the default value is 10000.
;	xrange: x plot range, default value is [min(x),max(x)]
;	yrange: y plot range, default value is [min(y),max(y)]
;	FITA: same as the parameter FITA in CURVEFIT, the default value FITA[*]=1 to force all the parameters to be fitted.
; EXAMPLE:
;	datadir='/home/ljt/ljt/data/AboutAGN/sample1/NGC0891/Chandra/profiles/data/'
;	number=103
;	data=fltarr(number,5)
;	openr,lun,datadir+'0.5-1.5keV_hp4hn4rp3rn3_SNR8_n103',/get_lun
;	readf,lun,data
;	close,lun
;	free_lun,lun
;	posi=where(data[*,0] ge 0.2, nposi)
;	x=fltarr(nposi)
;	y=fltarr(nposi)
;	erry=fltarr(nposi)
;	x[*]=data[posi,0]
;	y[*]=data[posi,3]
;	erry[*]=data[posi,4]
;	profilefit,x,y,err=erry,A=[30.,0.0,0.5,12.0],funname='fmexp',FITA=[1,0,1,1]
; EXAMPLE2:
;	$cp /home/ljt/ljt/data/AboutAGN/sample1/NGC0891/Chandra/profiles/Spitzer/data/I1_hp4hn4rp3rn3_SNR0_n487 n487
;	datadir='/home/ljt/ljt/data/AboutAGN/sample1/NGC0891/Chandra/profiles/data/'
;	number=64
;	data=fltarr(number,5)
;	openr,lun,datadir+'1.5-7keV_hp4hn4rp3rn3_SNR8_n64',/get_lun
;	readf,lun,data
;	close,lun
;	free_lun,lun
;	posi=where(abs(data[*,0]) le 3, nposi)
;	x=fltarr(nposi)
;	y=fltarr(nposi)
;	erry=fltarr(nposi)
;	x[*]=data[posi,0]
;	y[*]=data[posi,3]
;	erry[*]=data[posi,4]
;	A=fltarr(6)
;	maxwidth=0.1
;	Kprofile=487
;	Knumber=487
;	A[0]=3.
;	A[1]=1.
;	A[2]=12.
;	A[3]=Kprofile
;	A[4]=Knumber
;	A[5]=maxwidth
;	FITA=[1,1,0,0,0,0]
;	profilefit,x,y,err=erry,A=A,funname='fKband',FITA=FITA,pnum=100
;
; NOTE:
;	
;
;===============================================================
; HISTORY:
; Modified by ljt 2009/01/06: add xrange, yrange, FITA keywords.
;===============================================================
;-      

print,'; $Id: profilefit.pro,v 1.1 2009/01/06'
print,';'
print,'; Copyright (c), Li, Jiang Tao. All rights reserved.'
print,';       Unauthorized reproduction prohibited.'

if keyword_set(err) eq 0 then begin
	err=Y-Y
endif
if keyword_set(weights) eq 0 then begin
	weights=fltarr(n_elements(Y))
	weights[*]=1.0/float(n_elements(Y))
	;weights[*]=1.0
endif
if keyword_set(xtitle) eq 0 then begin
	xtitle='Minor-axis Distance (arcmin)'
endif
if keyword_set(ytitle) eq 0 then begin
	ytitle='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
endif
if keyword_set(pnum) eq 0 then begin
	pnum=10000
endif
if keyword_set(xrange) eq 0 then begin
	xrange=[min(X),max(X)]
endif
if keyword_set(yrange) eq 0 then begin
	yrange=[min(Y),max(Y)]
endif
if keyword_set(FITA) eq 0 then begin
	FITA=intarr(n_elements(A))
	FITA[*]=1
endif

;------------------------------------------------------------

fitfunname=funname+'fit'

yfit=CURVEFIT(X, Y, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME=fitfunname, FITA=FITA)

PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

window,0,xs=700,ys=500,retain=2
plot,x,y,psym=3,xtitle=xtitle,ytitle=ytitle,CHARSIZE=1.5,xrange=xrange,yrange=yrange,/ylog
oploterror,x,y,err,psym=3
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[min(Y),max(Y)]
Xpl=min(X)+(max(X)-min(X))/float(pnum)*float(findgen(pnum))
oplot,Xpl,CALL_FUNCTION(funname,Xpl,A),THICK=2

end



