pro hr_model,fname,ptv,pnv,ca,tv,nv,fa,noerase=noerase,thick=thick,xtitle=xtitle,ytitle=ytitle,hrch=hrch,pch=pch
;+
; plot model hardness ratio 1 or 2 vs hardness 
; 
; Definition:
; Hardness 1 = imdiv(c3+c4-c1-c2,c1+c2+c3+c4)
; Hardness 1 = imdiv(c2-c1,c1+c2)
; Hardness 2 = imdiv(c3-c2,c2+c3)
; where c1, c2, c3, and c4 are count rates in the four bands
;
; these rates are calculated with the script presented in memo of the directory
; $PUBDIR/xrayshell/rates/axaf/
;
; for raymond model:
; T=0.5, 1, 2, 4, 8 keV, 
; for power law model:
; photon index = 1, 2, and 3
; NH=1,100,300,1000, 2000 x 10^20 cm^{-2}
;
; model - right now two choices:
;	'vp' - power law
;	'vr' - raymond smith plasma
; ca - the 3-D output count rates as funtion of T(or index), N_H, and the band
; tv, nv - the spectral model parameters (power law index or T) and NH
; f1,f2,f3 - fluxes in the three bands 
; instr - 'aciss' or 'acisi' (def=!instr)
; hrdir - the directory containing the model count rate file
;	  def = '$PUBDIR/xrayshell/rates/axaf/'
; noerase - if set, the plot will be in the existing plot
; thick - the thickness of the curves.
; pch - if not = 1, reverse the X and Y axises
;
;*Example:
; hr_model,'vp'
;
; written by wqd, 6/27/2001
;
;-
if n_params() eq 0 then begin
print,'Calling Seq - hr_model,fname,ptv,pnv,ca,tv,nv,fa,noerase=noerase'
print,',thick=thick,xtitle=xtitle,ytitle=ytitle,hrch=hrch,pch=pch'
return
endif
if n_elements(pch) eq 0 then pch=1
if n_elements(hrch) eq 0 then hrch=1
nband=n_elements(fname)
read_mcntr,tv,nv,mc,flux=f,fname=fname(0)
nt=n_elements(tv)
ca=fltarr(nt,n_elements(nv),nband)
ca(*,*,0)=mc
fa=fltarr(nt,nband)
fa(*,0)=f
for k=1,nband-1 do begin
	read_mcntr,tv,nv,mc,flux=f,fname=fname(k)
	ca(*,*,k)=mc
	fa(*,k)=f
endfor
nv=nv*100.  
if n_elements(xtitle) eq 0 then xtitle='!6Hardness Ratio'
if n_elements(ytitle) eq 0 then ytitle='!6Hardness Ratio '+strtrim(hrch,2)

if n_elements(pnv) eq 0 then pnv=[3,10,30,100,300]
if n_elements(ptv) eq 0 then ptv=[0.3,1,3,10]
;tca=total(ca,3)
;tdca=total(ca(*,*,2:*),3)-total(ca(*,*,0:1),3)
tca=total(ca(*,*,1:*),3)
tdca=total(ca(*,*,2:*),3)-ca(*,*,1)
hra=imdiv(tdca,tca)
if nband eq 4 then hr1a=imdiv(ca(*,*,1)-ca(*,*,0),ca(*,*,0)+ca(*,*,1))
hr2a=imdiv(ca(*,*,nband-1)-ca(*,*,nband-2),total(ca(*,*,nband-2:nband-1),3))

if keyword_set(noerase) then overplot=1 else overplot=0
pnvs=[min(pnv),nv(where(nv ge min(pnv) and nv le max(pnv))),max(pnv)]
ptvs=[min(ptv),tv(where(tv ge min(ptv) and tv le max(ptv))),max(ptv)]
for k=0,n_elements(ptv)-1 do begin
	case hrch of 
            1: begin
                binterp,tv,nv,hr1a,pnvs*0.+ptv(k),pnvs,hrm
                binterp,tv,nv,hra,pnvs*0.+ptv(k),pnvs,hrsm
            end
            2: begin
                binterp,tv,nv,hr2a,pnvs*0.+ptv(k),pnvs,hrsm
		binterp,tv,nv,hra,pnvs*0.+ptv(k),pnvs,hrm
            end
	endcase
	if overplot eq 0 then begin
		if pch eq 1 then $
                  plot,hrm,hrsm,yrange=[-1,1],xrange=[-1,1], $
                  xtitl=xtitle,ytitl=ytitle,thick=thick $ 
                  else plot,hrsm,hrm,yrange=[-1,1],xrange=[-1,1], $
                  xtitl=xtitle,ytitl=ytitle,thick=thick 
		overplot=1
	endif else if pch eq 1 then oplot,hrm,hrsm,thick=thick $
          else oplot,hrsm,hrm,thick=thick
endfor 
if !debug eq 3 then stop,'stop 1'
for k=0,n_elements(pnv)-1 do begin
	case hrch of 
            1: begin
                binterp,tv,nv,hr1a,ptvs,ptvs*0.+pnv(k),hrm
		binterp,tv,nv,hra,ptvs,ptvs*0.+pnv(k),hrsm
            end
            2: begin
                binterp,tv,nv,hr2a,ptvs,ptvs*0.+pnv(k),hrsm
		binterp,tv,nv,hra,ptvs,ptvs*0.+pnv(k),hrm
            end
	endcase
	if overplot eq 0 then begin
		if pch eq 1 then $
                  plot,hrm,hrsm,yrange=[-1,1],xrange=[-1,1.2], $
                  xtitl=xtitle,ytitl=ytitle,thick=thick $ 
                  else plot,hrsm,hrm,yrange=[-1,1.2],xrange=[-1,1], $
                  xtitl=xtitle,ytitl=ytitle,thick=thick
                overplot=1
	endif else begin
            if pch eq 1 then $
              oplot,hrm,hrsm,thick=thick,line=2 $
            else oplot,hrsm,hrm,thick=thick,line=2
        endelse 
endfor 
if !debug eq 3 then stop
return
end
