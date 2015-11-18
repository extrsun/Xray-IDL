;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro ell_it,xp,yp,para,inpar=inpar,radius=radius,plimit=plimit,bpix=bpix,maxni=maxni,choice=choice,noverb=noverb,ww=ww
;+
; iteration to calculate the centroid, ellipicity, and orientation with 
; the momentum method; see Buate and Canizares, ApJ, 427, 86
; An interation is made for convergence.
; written by wqd, 9/25/96
;
; xp, yp - arrays containing pixel coordinates of individual counts
; para - output vector containing the x/y center position, ellipicity, 
;	and orientation (north through east)
; inpar - input vector containing initial parameter values.
; radius - initial radius within which counts are used (def= 3 arcmin)
; bpix - number of background counts per pixel
; plimit - conversion limit (percentage; def = 0.1,0.1,1.e-3,1.e-3)
; maxni - maximum number of interations to be allowed
; choice - if choice=1, only the converging of the x and y parameters are 
;	to be checked
; noverb - if set, on-screen output is to be minimized.
; ww - weights of individual counts (if provided) will be included in
; 	the momentum calculation.
;*EXAMPLE:
; ell_it,xss,yss,paras,radius=2.,bpix=bpix
;
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - ell_it,xp,yp,para,inpar=inpar,radius=radius'
print,',plimit=plimit,bpix=bpix,ww=ww'
return
endif
if n_elements(radius) eq 0 then radius=3. ;arcmin
if n_elements(maxni) eq 0 then maxni=1000
if n_elements(choice) eq 0 then choice = 0
;----------------------------------
if N_elements(plimit) eq 0 then plimit=[0.1,0.1,1.e-3,1.e-3]

hdim=!pref
;case !instr of
;  'p':  hdim=7679.5 
;  'h': 	hdim=4095.5
;endcase
xx=xp-hdim & yy=yp-hdim
if n_elements(inpar) eq 0 then inpar=[median(xx),median(yy),0.,0.]
rs=(radius*60./!size_pixel)^2

for ni=1,maxni do begin
	;get the counts within the source aperture
	qq=1.-inpar(2) > 0.1 ;arbitrary value to avoid diverging
	al=inpar(3)
	xe=cos(al)*(xx-inpar(0))+sin(al)*(yy-inpar(1))
	ye=-sin(al)*(xx-inpar(0))+cos(al)*(yy-inpar(1))
	res=xe^2+ye^2/qq^2
	sel=where(res le rs,cs)
	if cs eq 0 then begin
	 	print,'cs = 0'
		goto,done
	endif
	if n_elements(bpix) ne 0 then begin
		ell_mub,inpar(3),qq,mub
		mub=mub*rs
		bf=bpix*!pi*rs*qq/cs
	endif
	ell_para,xx(sel),yy(sel),para,mub=mub,bf=bf,ww=ww,status=status
	ratio=abs(para-inpar)/plimit
	if keyword_set(noverb) eq 0 then begin 
		print,'inpar =',inpar
		print,'para,ratio = ',para,ratio
	endif
	if choice eq 1 then ratio=ratio(0:1)
	if max(ratio) lt 1. or status eq -1 then goto,done else begin
		if choice eq 1 then begin
			inpar(0)=para(0)
			inpar(1)=para(1)
		endif else inpar=para	
	endelse
endfor
print,'maximum iteration exceeded!!!'
done:
para=[para,cs]
return
end
