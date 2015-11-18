pro fit_it,dv,nhv,ma,ttvo,parao,ttmin=ttmin,selo=selo,mbest=mbest $
,opfile=opfile,imd=imd,rhi=rhi,rlo=rlo,rd=rd,seln=seln,imsel=imsel,chn=chn $
,parav=parav,bchn=bchn
;+
; dv,nhv - vectors containing data (intensity) and absorbing column 
;	(10^20 cm^-2)
; ma - array containing model vectors at various temperatures
; ttvo	- vector containing the temperatures of ma
; parao - output parameters (e.g., the extragalactic background and 
;	the density square if chn = 0
; ttmin - initial guess of the temperature at the Sun location (def = 1.5);
;	as output containing the best-fit temperature
; selo - locations of selected pixels in the vectors
; mbest - best-fit model vector at the final threshold
; opfile - the opacity file for the absorption of the extragalactic background
;	the abundance should be consistent with that used for deriving ma
;	def = power law of energy slope = 1 and solar abundance
; imd - data image for comparison purpose and for array size information only
; rhi, rlo, rd - the upper, lower, and fraction decrement of the differential
;		ratio (def = 2, 0.3, 0.9)
; seln - output locations of the final remaining data pixels
; imsel - the location of individual elements in the vectors
; chn - choices (def = 0). If =1, a local component is included in the fit
; parav - parameters (the number of remaining pixels, the pixel-removing
;	threshold, the temperature, the density square, and the 
;	"Chi^2" value (sum[(d-m)/m]^2) in all iterations
; bchn - band choince (def bchn=1 R4-5 band; =0 R1-2 band; =2 R6-7 band
; written by wqd 3/4/97 to decompose the ROSAT all-sky survey map
;-
common func_param,choice,dvs,mas,ttv,wws,ops,para,tcr,masn,nb
mit=500
if n_elements(chn) ne 0 then choice=chn else choice=0
if n_elements(rlo) eq 0 then rlo=0.3
if n_elements(rhi) eq 0 then rhi=2.
if n_elements(rd) eq 0 then rd=0.9
if n_elements(ttmin) eq 0 then ttmin=1.5
ttv=ttvo
if n_elements(bchn) eq 0 then bchn=1 ;4-5 band
dvsz=size(dv)
if dvsz(0) eq 2 then nb=dvsz(2) else nb=1
if choice eq 1 then parav=fltarr(7,mit) else parav=fltarr(5+nb,mit)

op=fltarr(dvsz(1),nb)
if n_elements(opfile) eq 0 then opfile='/home/wqd/rosatdata/shadow/test.'
for k=0,nb-1 do begin
  sh_opac,nhv*0.01>0.,op1,bchn+k,opf=opfile,/nolog,/noim 
		;only for R4-5 and  R6-7
  op(*,k)=op1
endfor

ratioth=rhi
if n_elements(selo) eq 0 then selo=lindgen(dvsz(1))
nsel=n_elements(selo)
seln=selo
;wws=fltarr(nsel,nb)+1.
wws=1./(dv(selo,*)^2> 1.e-10)
func='func_powell'
for k=1,mit do begin
 repeat begin
	nselo=nsel
	dvs=dv(seln,*)
	ops=op(seln,*)
	mas=ma(seln,*,*)
	xa=ttmin & xb=xa*1.1 ;bracket the minimum temperature range
	minf_bracket,xa,xb,xc,fa,fb,fc,func=func
	minf_parabolic,xa,xb,xc,ttmin,fmin,func=func
		;fit the parameters. Both the fe and ne^2 are transfered 
		;through common block
	chi=call_function(func,ttmin) ;make sure the parameters in common block
					;represent the best-fit values
;	wws=1./sqrt(tcr > 1.e-10)
	wws=1./(tcr > 1.e-10)
;	wws=1./(dvs > 1.e-10)
	ratioa=abs((dvs-tcr)*wws)
	ratio=ratioa(*,0)
	if nb gt 1 then for kb=1,nb-1 do ratio=ratio > ratioa(*,kb)
if !debug eq 3 then begin
if nb eq 2 then begin
	print,'ttmin,chi,para = ',ttmin,chi,para
   	imm=imd*0.
	imm(imsel(seln))=tcr(*,0)
 	tv,bscale(imm,0.,5),k
 	imm(imsel(seln))=tcr(*,1)
 	tv,bscale(imm,0.,5),k+1
	imdn=imd*0.
	imdn(imsel(seln))=dv(seln,0)
	tv,bscale(imdn,0,5),k+2
	imdn=imd*0.
	imdn(imsel(seln))=dv(seln,1)
	tv,bscale(imdn,0,5),k+3
 	if !debug eq 2 then stop
endif
endif
	ratioth=ratioth > rlo*0.9999 ;so ratioth will not be much below rlo
	ss=where(ratio le ratioth,nsel)
	seln=seln(ss)
	wws=wws(ss,*)^2
 endrep until nsel eq nselo
 imm=imd*0.
 imm(imsel(seln))=tcr(ss,0)
 tv,bscale(imm,0.,5),k
 parav(*,k-1)=[nsel,ratioth,ttmin,para,chi]
 print,'ttmin,fmin,para = ',ttmin,fmin,para
 print,'k, ratioth, chi, nsel = ', k, ratioth, chi, nsel
 if !debug eq 2 then stop
 if k eq 1 then ratioth=ratioth < max(ratio)*rd
 if ratioth lt rlo then begin
	imdn=imd*0.
	imdn(imsel(seln))=imd(imsel(seln))
	tv,bscale(imdn,0,5)
	stop,'want to change rlo?'
 endif
 if ratioth ge rlo then ratioth=ratioth*rd else goto,done
endfor
done:
chi=call_function(func,ttmin) ;with the new wws
mbest=tcr
parav=parav(*,0:k-1)
parao=para

return
end
	
;===================================================================
function func_powell,tt
common func_param,choice,dvs,mas,ttv,wws,ops,para,tcr,masn,nb
nttv=n_elements(ttv)
if tt gt ttv(nttv-1) or (tt lt ttv(0)) then chi=1.e20 $
		; effective set the boundaries of the model parameters
	else begin
	get_masn,mas,ttv,tt,masn,nb=nb
	fit_linear,dvs,ops,masn,wws,para,choice=choice,nb=nb
	get_tcr,para,masn,ops,tcr,choice=choice,nb=nb
	chi=total((dvs-tcr)^2*wws)
endelse
if !debug eq 1 then print,'powell ',chi,tt,para
return,chi
end

;=============================================================
pro fit_linear,dv,op,ma,ww,para,choice=choice,nb=nb

if n_elements(choice) eq 0 then choice=0

dvww=dv*ww
tdop=fltarr(nb)
for k=0,nb-1 do tdop(k)=total(dvww(*,k)*op(*,k))
tdp=total(dvww*ma)

opww=op*ww
top2=fltarr(nb)
topp=fltarr(nb) 
for k=0,nb-1 do begin
	topp(k)=total(opww(*,k)*ma(*,k))
	top2(k)=total(op(*,k)*opww(*,k))
endfor

 bb=tdp
 aa=fltarr(nb+1,nb+1)
 aa(nb,nb)=total(ww*ma^2)
 for k=nb-1,0,-1 do begin
	bb=[tdop(k),bb]
	aa(k,k)=top2(k)
	aa(nb,k)=topp(k)
	aa(k,nb)=topp(k)
 endfor
 para=cramer(aa,bb,/double)
if !debug eq 1 then stop
return
end
;===================================================================
pro get_masn,mas,ttv,tt,masn,nb=nb
if n_elements(nb) eq 0 then nb=1
tabinv,ttv,tt,indlo
indlo=fix(indlo) < (n_elements(ttv)-2)
indhi=indlo+1
 masn=((ttv(indhi)-tt)/(ttv(indhi)-ttv(indlo)))*mas(*,indlo,*)+ $
 ((tt-ttv(indlo))/(ttv(indhi)-ttv(indlo)))*mas(*,indhi,*) 
if nb ne 1 then masn=reform(masn,n_elements(masn)/nb,nb)
return
end
;========================================
pro get_tcr,para,masn,ops,tcr,choice=choice,nhv=nhv,opfile=opfile,bchn=bchn,nb=nb
;+
; add the components up
;-
if n_elements(nb) eq 0 then nb=1
if n_elements(bchn) eq 0 then bchn=1 ;4-5 band
if n_elements(nhv) ne 0 then begin
 ops=fltarr(n_elements(nhv),nb)
 if n_elements(opfile) eq 0 then opfile='/home/wqd/rosatdata/shadow/test.'
 for k=0,nb-1 do begin
  sh_opac,nhv*0.01>0.,op1,bchn+k,opf=opfile,/nolog,/noim
  ops(*,k)=op1
 endfor
endif
	if nb eq 1 then tcr=para(0)*ops+para(1)*masn else begin
		npara=n_elements(para)
		tcr=para(npara-1)*masn
		for kb=0,nb-1 do tcr(*,kb)=tcr(*,kb)+para(kb)*ops(*,kb)
	endelse
return
end
;==============================================
pro cr_gal_fit_it,dv,nhv,ma,para,chi,selo=selo,mbest=mbest,opfile=opfile,imd=imd,rhi=rhi,rlo=rlo,rd=rd,seln=seln,imsel=imsel,weight=weight,kmin=kmin
;+
; similar to fit_it, but the temperature is not fitted. instead the best-fit
; is calculated in each individual temperatures provided.
;-
if n_elements(rlo) eq 0 then rlo=0.3
if n_elements(rhi) eq 0 then rhi=0.9
if n_elements(rd) eq 0 then rd=0.9
if n_elements(mit) eq 0 then mit=50
ratioth=rhi
nsel=n_elements(selo)
seln=selo
ww=1.
for k=1,mit do begin
 repeat begin
	nselo=nsel
	if keyword_set(weight) ne 0 then begin
		ww=ww^2
		cr_gal_fitw,dv(seln),nhv(seln),ma(seln,*),ww,para,chi $
			,mbest=mbest,opfile=opfile,kmin=kmin 
		ww=1./(mbest > 1.e-10)
		ratio=(dv(seln)-mbest)*ww
	endif else begin
		cr_gal_fit,dv(seln),nhv(seln),ma(seln,*),ttv,ne2,fe,chi $
			,mbest=mbest,opfile=opfile
		ratio=imdiv(dv(seln)-mbest,mbest)
	endelse
	ss=where(abs(ratio) le ratioth,nsel)
	seln=seln(ss)
	ww=ww(ss)
 endrep until nsel eq nselo

 imm=imd*0.
 imm(imsel(seln))=mbest(ss)
 tv,bscale(imm,0.,5),k
 if !debug eq 2 then stop
 if ratioth lt rlo then begin
	imdn=imd*0.
	imdn(imsel(seln))=imd(imsel(seln))
	tv,bscale(imdn,0,5)
	stop,'want to change rlo?'
 endif
 if ratioth ge rlo then ratioth=ratioth*rd else goto,done
print,'k, ratioth, nsel = ', k, ratioth, nsel
endfor
done:
return
end
	

