;------------------------------------------------------------
FUNCTION gauss,X,A
  bx1=A[0]*EXP(-(X-A[1])^2/(A[2])^2/2.)
  F=bx1+A[3]
return,F
END
;------------------------------------------------------------
FUNCTION gaussM,X,A
  bx1=A[0]*EXP(-(X)^2/(A[1])^2/2.)
  F=bx1+A[2]
return,F
END
;------------------------------------------------------------

FUNCTION f2gauss,X,A
  bx1=A[0]*EXP(-(X-A[1])^2/(A[2])^2/2.)
  bx2=A[3]*EXP(-(X-A[4])^2/(A[5])^2/2.)
  F=bx1+bx2+A[6]
return,F
END
;------------------------------------------------------------
FUNCTION f2gaussM,X,A
  bx1=A[0]*EXP(-(X)^2/(A[1])^2/2.)
  bx2=A[2]*EXP(-(X-A[3])^2/(A[4])^2/2.)
  F=bx1+bx2+A[5]
return,F
END
;------------------------------------------------------------

FUNCTION fmexp,X,A
  Ig=A[0]
  z0=A[1]
  zh=A[2]
  F=Ig*exp(-abs(X-z0)/zh)+A[3]
return,F
END

;------------------------------------------------------------
FUNCTION fexp,X,A
  Ig=A[0]
  zh=A[1]
  F=Ig*exp(-abs(X)/zh)+A[2]
return,F
END
;------------------------------------------------------------
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
FUNCTION fconstexp2,X,A
  I1=A[0]
  z1=A[1]
  const=A[2]
  F=fltarr(n_elements(X))
 for i=0,n_elements(X)-1 do begin
   F[i]=I1*exp(-abs(X[i])/z1)+const
 endfor
return,F
END
;------------------------------------------------------------
FUNCTION fnobckexp,X,A
  I1=A[0]
  z1=A[1]
  I2=A[2]
  z2=A[3]
  F=fltarr(n_elements(X))
 for i=0,n_elements(X)-1 do begin
  if X[i] lt 0 then begin
   F[i]=I1*exp(X[i]/z1)
  endif
  if X[i] ge 0 then begin
   F[i]=I2*exp(-X[i]/z2)
  endif
 endfor
return,F
END
;------------------------------------------------------------
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
FUNCTION fnobckconstIexp2,X,A
  It=A[0]
  z1=A[1]
  F=fltarr(n_elements(X))
 for i=0,n_elements(X)-1 do begin
  F[i]=It*exp(abs(X[i]/z1))
 endfor
return,F
END
;------------------------------------------------------------
FUNCTION Kfun,Kprofile,Xlow,Xhigh,number,width
 data=fltarr(3,number)
 openr,lun,Kprofile,/get_lun
  readf,lun,data
 close,lun
 free_lun,lun
 subscript=where((data[0,*] ge Xlow) and (data[1,*] le Xhigh),totalnumber)
 if totalnumber gt 0 then begin
  return,total(data[2,subscript]*(abs(data[1,subscript]-data[0,subscript])*width))/(abs(Xhigh-Xlow)*width)
 endif else begin
  ;print,'range=',Xlow,Xhigh
  ;return,0
 endelse
END
;------------------------------------------------------------
FUNCTION fKband,X,A,maxwidth,Kprofile,Knumber,width
;maxwidth is the maximum width to average the K band intensity in X direction for each data point; Kprofile is the K band luminosity profile; Knumber is the number of data points in Kprofile; width is the width perpendicular to x direction to get the profile.
;Xl-Xb=norm*K^alpha
  norm=A[0]
  alpha=1.0
  ;Xb=A[1]
  F=fltarr(n_elements(X))
 for i=0,n_elements(X)-1 do begin
  if i gt 0 and i lt n_elements(X)-1 then begin
   xwidth=(maxwidth<(abs(X[i+1]-X[i-1])/2.))
  endif else begin
   xwidth=maxwidth
  endelse
  F[i]=norm*(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber,width))^alpha;+Xb
 endfor
return,F
END
;------------------------------------------------------------
FUNCTION Kfun2,Kprofile,Xlow,Xhigh,number,ellipticity
 data=fltarr(3,number)
 openr,lun,Kprofile,/get_lun
  readf,lun,data
 close,lun
 free_lun,lun
 subscript=where((data[0,*] ge Xlow) and (data[1,*] le Xhigh),totalnumber)
 if totalnumber gt 0 then begin
  return,total(data[2,subscript]*(!pi*(data[1,subscript]^2-data[0,subscript]^2)*ellipticity))/(!pi*(Xhigh^2-Xlow^2)*ellipticity)
 endif else begin
  ;print,'range=',Xlow,Xhigh
  ;return,0
 endelse
END
;------------------------------------------------------------
FUNCTION fKband2,X,A,maxwidth,Kprofile,Knumber,ellipticity
;maxwidth is the maximum width to average the K band intensity in X direction for each data point; Kprofile is the K band luminosity profile; Knumber is the number of data points in Kprofile; ellipticity is the ellipticity of the annulus.
;Xl-Xb=norm*K^alpha
  norm=A[0]
  alpha=1.0
  ;Xb=A[1]
  F=fltarr(n_elements(X))
 for i=0,n_elements(X)-1 do begin
  if i gt 0 and i lt n_elements(X)-1 then begin
   xwidth=(maxwidth<(abs(X[i+1]-X[i-1])/2.))
  endif else begin
   xwidth=maxwidth
  endelse
  F[i]=norm*(Kfun2(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber,ellipticity))^alpha;+Xb
 endfor
return,F
END

;============================================================

PRO fgaussfit, X, A, F, pder

  bx1=A[0]*EXP(-(X-A[1])^2/(A[2])^2/2.)
  F=bx1+A[3]

  pder=fltarr(n_elements(X),4)
  pder[*,0]=bx1/A[0]
  pder[*,1]=bx1*(X-A[1])/A[2]
  pder[*,2]=bx1*(X-A[1])^2/(A[2])^3
  pder[*,3]=replicate(1.0, N_ELEMENTS(X))

END
;------------------------------------------------------------

PRO fgaussMfit, X, A, F, pder

  bx1=A[0]*EXP(-(X)^2/(A[1])^2/2.)
  F=bx1+A[2]

  pder=fltarr(n_elements(X),3)
  pder[*,0]=bx1/A[0]
  pder[*,1]=bx1*(X-A[1])^2/(A[2])^3
  pder[*,2]=replicate(1.0, N_ELEMENTS(X))

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
;------------------------------------------------------------
PRO f2gaussMfit, X, A, F, pder

  bx1=A[0]*EXP(-(X)^2/(A[1])^2/2.)
  bx2=A[2]*EXP(-(X-A[3])^2/(A[4])^2/2.)
  F=bx1+bx2+A[5]

  pder=fltarr(n_elements(X),6)
  pder[*,0]=bx1/A[0]

  pder[*,1]=bx1*(X)^2/(A[1])^3
  pder[*,2]=bx2/A[2]
  pder[*,3]=bx2*(X-A[3])/A[4]
  pder[*,4]=bx2*(X-A[3])^2/(A[4])^3
  pder[*,5]=replicate(1.0, N_ELEMENTS(X))

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

;------------------------------------------------------------
PRO fexpfit, X, A, F, pder

  Ig=A[0]
  zh=A[1]
  F=Ig*exp(-abs(X)/zh)+A[2]

  pder=fltarr(n_elements(X),3)
  pder[*,0]=exp(-abs(X)/zh)
  pder[*,1]=abs(Ig*(X)/zh^2)*exp(-abs(X)/zh)
  pder[*,2]=replicate(1.0, N_ELEMENTS(X))

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
;------------------------------------------------------------
PRO fconstexpfit2, X, A, F, pder

  I1=A[0]
  z1=A[1]
  const=A[2]
  F=fltarr(n_elements(X))
  pder=fltarr(n_elements(X),n_elements(A))
 for i=0, n_elements(X)-1 do begin
  F[i]=I1*exp(-abs(X[i])/z1)+const
  pder[i,0]=exp(-abs(X[i])/z1)
  pder[i,1]=I1*abs(X[i])/z1^2*exp(-abs(X[i])/z1)
 endfor
  pder[*,2]=replicate(1.0, N_ELEMENTS(X))

END
;------------------------------------------------------------
PRO fnobckexpfit, X, A, F, pder

  I1=A[0]
  z1=A[1]
  I2=A[2]
  z2=A[3]
  F=fltarr(n_elements(X))
  pder=fltarr(n_elements(X),n_elements(A))
 for i=0, n_elements(X)-1 do begin
  if X[i] lt 0 then begin
   F[i]=I1*exp(X[i]/z1)
  endif
  if X[i] ge 0 then begin
   F[i]=I2*exp(-X[i]/z2)
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
;------------------------------------------------------------
PRO fnobckconstIexpfit2, X, A, F, pder

  It=A[0]
  z1=A[1]
  F=fltarr(n_elements(X))
  pder=fltarr(n_elements(X),n_elements(A))
 for i=0, n_elements(X)-1 do begin
   F[i]=It*exp(X[i]/z1)
   pder[i,0]=exp(X[i]/z1)
   pder[i,1]=-It*X[i]/z1^2*exp(X[i]/z1)
 endfor

END
;------------------------------------------------------------
PRO fKbandfit, X, A, F, pder
;Xl=norm*K^alpha+Xb

maxwidth=0.5
Kprofile='/home/ljt/ljt/data/AboutAGN/NGC5866/image/multiprofile/radialprofile_forsherpafit_K_n295_minor'
Knumber=295
width=1.6

  norm=A[0]
  alpha=1.0
  ;Xb=A[1]
  F=fltarr(n_elements(X))
  pder=fltarr(n_elements(X),n_elements(A))
 for i=0,n_elements(X)-1 do begin
  if i gt 0 and i lt n_elements(X)-1 then begin
   xwidth=(maxwidth<(abs(X[i+1]-X[i-1])/2.))
  endif else begin
   xwidth=maxwidth
  endelse
  F[i]=norm*(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber,width))^alpha;+Xb
  pder[i,0]=(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber,width))^alpha
  ;pder[i,1]=norm*alpha*(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber,width))^(alpha-1)
 endfor
  ;pder[*,1]=replicate(0.0, N_ELEMENTS(X))

END
;------------------------------------------------------------
PRO fKbandfit2, X, A, F, pder
;Xl=norm*K^alpha+Xb

maxwidth=0.5
Kprofile='/home/ljt/ljt/data/AboutAGN/NGC5866/image/multiprofile2/radialprofile_forsherpafit_K_n342_radial'
Knumber=342
ellipticity=1.9/4.7

  norm=A[0]
  alpha=1.0
  ;Xb=A[1]
  F=fltarr(n_elements(X))
  pder=fltarr(n_elements(X),n_elements(A))
 for i=0,n_elements(X)-1 do begin
  if i gt 0 and i lt n_elements(X)-1 then begin
   xwidth=(maxwidth<(abs(X[i+1]-X[i-1])/2.))
  endif else begin
   xwidth=maxwidth
  endelse
  F[i]=norm*(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber,ellipticity))^alpha;+Xb
  pder[i,0]=(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber,ellipticity))^alpha
  ;pder[i,1]=norm*alpha*(Kfun(Kprofile,X[i]-xwidth/2.,X[i]+xwidth/2.,Knumber,ellipticity))^(alpha-1)
 endfor
  ;pder[*,1]=replicate(0.0, N_ELEMENTS(X))

END

;============================================================


pro radialprofilefit,X,Y,err,bckgrndlev=bckgrndlev,A=A,funname=funname

weights=fltarr(n_elements(Y))
;weights[*]=1.0/float(n_elements(Y))
weights[*]=1.0

;------------------------------------------------------------
if strcmp(funname,'fgaussMfit') then begin
;Modified gauss fit
;A=[30.0,-0.15,3.0]

yfit=CURVEFIT(X, Y, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='fgaussMfit')

PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

window,0,xs=700,ys=500,retain=2
tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
!color=0
ploterr,x,y,err,psym=3,background=255B,CHARSIZE=1.5,yrange=[0,60],xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[0,60]
!color=2
oplot,X,gaussM(X,A),THICK=2

endif


;------------------------------------------------------------
if strcmp(funname,'f2gaussfit') then begin
;two gauss fit
;A=[30.0,-0.15,0.3,10.0,0.0,1.0,2.0]

yfit=CURVEFIT(X, Y, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='f2gaussfit')
;YERROR=yerr
PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

A1=fltarr(4)
A2=fltarr(4)
A1[0:2]=A[0:2]
A2[0:2]=A[3:5]
A1[3]=A[6]
A2[3]=A[6]

window,0,xs=700,ys=500,retain=2
tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
!color=0
ploterr,x,y,err,psym=3,background=255B,CHARSIZE=1.5,yrange=[0,60],xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[0,60]
!color=2
oplot,X,f2gauss(X,A),THICK=2
X=float(findgen(100))/20.-2.
!color=4
oplot,X,gauss(X,A1),LINESTYLE=2,THICK=2
!color=3
oplot,X,gauss(X,A2),LINESTYLE=4,THICK=2

endif

;------------------------------------------------------------
if strcmp(funname,'f2gaussMfit') then begin
;Modified two gauss fit
;A=[30.0,0.3,10.0,0.0,1.0,2.0]

yfit=CURVEFIT(X, Y, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='f2gaussMfit')
;YERROR=yerr
PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA      ;/SQRT(CHISQ/(n_elements(X)*n_elements(A)))
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

A1=fltarr(4)
A2=fltarr(4)
A1[0]=A[0]
A1[1]=0.0
A1[2]=A[1]
A2[0:2]=A[2:4]
A1[3]=A[5]
A2[3]=A[5]

window,0,xs=700,ys=500,retain=2
tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
!color=0
ploterr,x,y,err,psym=3,background=255B,CHARSIZE=1.5,yrange=[0,60],xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[0,60]
!color=2
oplot,X,f2gaussM(X,A),THICK=2
X=float(findgen(100))/20.-2.
!color=4
oplot,X,gauss(X,A1),LINESTYLE=2,THICK=2
!color=3
oplot,X,gauss(X,A2),LINESTYLE=4,THICK=2

endif

;------------------------------------------------------------
if strcmp(funname,'fexpfit') then begin
;expotential fit
;A=[10.0,0.0,0.3,0.0]
;A=[20.0,0.3,6.0]
yfit=CURVEFIT(X, Y, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='fexpfit')

PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

A1=fltarr(3)
A1[0:2]=A[0:2]

window,0,xs=700,ys=500,retain=2
tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
!color=0
plot,x,y,psym=4,background=255B,/ylog,xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
oploterror,x,y,err,psym=3,CHARSIZE=1.5
oplot,[0,0],[0,60]
!color=2
Xpl=float(findgen(10000))/1000.-2.
oplot,Xpl,fexp(Xpl,A),THICK=2

endif
;------------------------------------------------------------
if strcmp(funname,'fconstexpfit') then begin
;constant background expotential fit
;A=[30.0,0.5,30.0,0.5,6.95656]
yfit=CURVEFIT(X, Y, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='fconstexpfit')

PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

window,0,xs=700,ys=500,retain=2
tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
!color=0
plot,x,y,psym=3,background=255B,/ylog,xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
oploterror,x,y,err,psym=3,CHARSIZE=1.5
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[0,60]
!color=2
Xpl=float(findgen(13750))/1250.-5.5
oplot,Xpl,fconstexp(Xpl,A),THICK=2

endif
;------------------------------------------------------------
if strcmp(funname,'fconstexpfit2') then begin
;constant background expotential fit
;A=[30.0,0.5,6.95656]
yfit=CURVEFIT(X, Y, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='fconstexpfit2')

PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

window,0,xs=700,ys=500,retain=2
tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
!color=0
plot,x,y,psym=3,background=255B,/ylog,xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
oploterror,x,y,err,psym=3,CHARSIZE=1.5
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[0,60]
!color=2
Xpl=float(findgen(13750))/1250.-5.5
oplot,Xpl,fconstexp2(Xpl,A),THICK=2

endif
;------------------------------------------------------------
if strcmp(funname,'fnobckexpfit') then begin
;sub constant background expotential fit
;A=[200.0,0.15,200.0,0.3]
yfit=CURVEFIT(X, Y-bckgrndlev, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='fnobckexpfit')

PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

window,0,xs=700,ys=500,retain=2
tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
!color=0
plot,x,y,psym=3,background=255B,/ylog,xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
oploterror,x,y,err,psym=3,CHARSIZE=1.5
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[0,60]
!color=2
Xpl=float(findgen(10000))/1250.-4.
oplot,Xpl,fnobckexp(Xpl,A)+bckgrndlev,THICK=2

endif
;------------------------------------------------------------

if strcmp(funname,'fnobckconstIexpfit2') then begin
;sub constant background constant I expotential fit
;A=[30.0,0.3]
yfit=CURVEFIT(X, Y-bckgrndlev, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='fnobckconstIexpfit2')

PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

window,0,xs=700,ys=500,retain=2
tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
!color=0
plot,x,y,psym=3,background=255B,/ylog,xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
oploterror,x,y,err,psym=3,CHARSIZE=1.5
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[0,60]
!color=2
Xpl=float(findgen(10000))/1250.-4.
oplot,Xpl,fnobckconstIexp2(Xpl,A)+bckgrndlev,THICK=2

endif


;------------------------------------------------------------
if strcmp(funname,'fKbandfit') then begin
;K band profile fit, without X-ray background subtracted
;A=[0.5,1.0,4.26189]
;NOTE THAT WHEN USE THIS FUNCTION, THE FOLLOWING PARAMETERS SHOULD BE PROVIDED IN BOTH FKBANDFIT.PRO AND HERE.
maxwidth=0.5
Kprofile='/home/ljt/ljt/data/AboutAGN/NGC5866/image/multiprofile/radialprofile_forsherpafit_K_n295_minor'
Knumber=295
width=1.6

yfit=CURVEFIT(X, Y, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='fKbandfit')

PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

window,0,xs=700,ys=500,retain=2
;tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
;!color=0
plot,x,y,psym=3,background=255B,xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)'
oploterror,x,y,err,psym=3,CHARSIZE=1.5
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[0,60]
;!color=2
Xpl=float(findgen(100))/12.5-4.
oplot,Xpl,fKband(Xpl,A,maxwidth,Kprofile,Knumber,width),THICK=2

endif
;------------------------------------------------------------
if strcmp(funname,'fKbandfit2') then begin
;K band profile fit, without X-ray background subtracted
;A=[0.5,1.0,4.26189]
;NOTE THAT WHEN USE THIS FUNCTION, THE FOLLOWING PARAMETERS SHOULD BE PROVIDED IN BOTH FKBANDFIT.PRO AND HERE.
maxwidth=0.5
Kprofile='/home/ljt/ljt/data/AboutAGN/NGC5866/image/multiprofile2/radialprofile_forsherpafit_K_n342_radial'
Knumber=342
ellipticity=1.9/4.7

yfit=CURVEFIT(X, Y, weights, A, SIGMA, YERROR=yerr, CHISQ=CHISQ, FUNCTION_NAME='fKbandfit2')

PRINT, 'Function parameters: ', A
PRINT, 'SIGMA: ', SIGMA
PRINT, 'CHISQ: ', CHISQ
PRINT, 'YERROR: ', yerr

window,0,xs=700,ys=500,retain=2
;tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
;!color=0
yrange=[1e-3,1e2]
plot,x,y,psym=3,background=255B,xtit='Minor-axis Distance (arcmin)',ytit='!6X-ray Intensity (10!U-4!N count s!U-1!N arcmin!U-2!N)',yrange=yrange,/ylog
oploterror,x,y,err,psym=3,CHARSIZE=1.5
oplot,x,y,psym=4,SYMSIZE=2
oplot,[0,0],[0,60]
;!color=2
Xpl=float(findgen(50))/12.5+0.3
oplot,Xpl,fKband2(Xpl,A,maxwidth,Kprofile,Knumber,ellipticity),THICK=2

endif

end



