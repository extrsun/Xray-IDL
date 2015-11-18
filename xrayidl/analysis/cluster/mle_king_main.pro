;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro mle_king_main,list,xp,yp,expt,bpix,sigma=r_core,bet=bet $
,inpar=inpar,para=para,radiusm=radiusm,chn=chn,verb=verb,ffdd=ffdd,io=io, $
fixpara=fixpara,xx=xx,yy=yy
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - mle_king_main,list,xp,yp,expt,bpix,sigma=r_core'
print,',bet=bet,inpar=inpar,para=para,radiusm=radiusm,chn=chn,verb=verb'
print,',ffdd=ffdd,io=io,fixpara=fixpara,xx=xx,yy=yy'
return
endif
common fix,fixpv
common feedback,bfac,bfac2,rrsc2,rcs
if n_elements(radiusm) eq 0 then radiusm=5. ;arcmin
if n_elements(chn) eq 0 then chn=1
if n_elements(maxni) eq 0 then maxni=1000
if n_elements(fixpara) ne 0 then fixpv=fixpara
;----------------------------------
radius=radiusm*120. ;in units of pixels
bc=bpix*double(radius)^2*!pi 

hdim=!pref
;case !instr of
;  'p':  hdim=7680.5 ;FORTRAN Position as in the count list
;  'h': 	hdim=4096.5
;endcase
if n_elements(xx) eq 0 then begin
    xx=list.x-hdim & yy=list.y-hdim
endif
if n_elements(inpar) ne 0 then begin
	xo=inpar(0) & yo=inpar(1) 
endif else begin
	xo=xp & yo=yp
endelse 
for ni=1,maxni do begin
	;get the counts within the source aperture
	sel=where(((xx-xo)^2+(yy-yo)^2) le radius^2,cs)
	mle_king,xx(sel),yy(sel),bc,radius,para,xo=xo,yo=yo,bet=bet $
	 ,r_core=r_core,fmin=fmin,ffdd=ffdd,chn=chn,verb=verb,tc=cs,inpar=inpar
	deltadis=sqrt((para(0)-xo)^2+(para(1)-yo)^2)
	print,'para,fmin,deltadis = ',para,fmin,deltadis
	if deltadis lt 0.5 then goto,done else begin
		inpar=para	
		xo=para(0)
		yo=para(1)
	endelse
endfor

done:
print,'para:',para,fmin
np=n_elements(para)
print,'90% errors:',sqrt(ffdd(indgen(np),indgen(np))*2.706) 
print,'cs,bc = ',cs,bc
print,'bfac = ',bfac
if chn eq 5 then print,'bfac2,cdif,re =',bfac2,sqrt(rcs)*para(3),sqrt(rrsc2)*para(3)
;ibetam,3.*para(2)-1.5,radius^2/(radius^2+para(3)^2),bfac
Io=!pi*para(3)^2*bfac
Io=(cs-bc)/Io*(60/!size_pixel)^2/expt
print,'Io = ',Io,' cts/s arcmin^2'
return
end
