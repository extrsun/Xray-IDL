pro ell_para_plot,pp,pm,n,r1=r1,r2=r2,xys=xys
;+
; Plot elliptical parameters as function of semi-major axis
; pp - the best fits from the data
; pp - fits from the simulated data
; r1, r2 - lower and upper limits of the scale rows to be included in the plot 
;	(the largest scale = 0)
; xys - offsets for x, y and ellipticity (def =[0,0,0]).
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - ell_para_plot,pp,pm,n,r1=r1,r2=r2,xys=xys'
return
endif
if n_elements(xys) eq 0 then xys=[0,0,0]
if n_elements(r1) eq 0 then r1=0
if n_elements(r2) eq 0 then r2=n-1
rr=(reverse(findgen(n))+1)

!p.charsize=1.2
!p.thick=1.5
!p.multi=[0,2,2,0,0]
!x.style=1
k=0
xrange=[-0.5+min(rr),max(rr)+0.5]
yrange=[min(0.5*(pm(k,r1:r2,1)-xys(k))), max(0.5*(pm(k,r1:r2,2)-xys(k)))]
yt=['R.A. shift (arcsec)','Dec. shift (arcsec)','Ellipticity']
plot,rr,0.5*(pp(k,r1:r2)-xys(k)),psym=6,xtitle='Semi-major axis (arcmin)',ytit=yt(k),xrange=xrange,yrange=yrange
errplot_x,rr,0.5*(pm(k,r1:r2,1)-xys(k)),0.5*(pm(k,r1:r2,2)-xys(k))

k=1
yrange=[min(0.5*(pm(k,r1:r2,1)-xys(k))), max(0.5*(pm(k,r1:r2,2)-xys(k)))]
plot,rr,0.5*(pp(k,r1:r2)-xys(k)),psym=6,xtitle='Semi-major axis (arcmin)',ytit=yt(k),xrange=xrange,yrange=yrange
errplot_x,rr,0.5*(pm(k,r1:r2,1)-xys(k)),0.5*(pm(k,r1:r2,2)-xys(k))

k=2
yrange=[min((pm(k,r1:r2,1)-xys(k))), max((pm(k,r1:r2,2)-xys(k)))]
plot,rr,pp(k,r1:r2)-xys(k),psym=6,xtitle='Semi-major axis (arcmin)',ytit=yt(k),xrange=xrange,yrange=yrange
s=pm(k,r1:r2,1)
if min(s) lt 0.1 then s(where(s lt 0.1))=0.
errplot_x,rr,s-xys(k),pm(k,r1:r2,2)-xys(k)

k=3
s=pp(k,r1:r2,0)*180./!pi+90.
s1=pm(k,r1:r2,1)*180./!pi+90.
s2=pm(k,r1:r2,2)*180./!pi+90.
sel=where(s gt 90.,nsel)
if nsel ne 0 then begin
	s(sel)=s(sel)-180.
	s1(sel)=s1(sel)-180.
	s2(sel)=s2(sel)-180.
endif
yrange=[min(s1), max(s2)]
plot,rr,s,psym=6,xtit='Semi-major axis (arcmin)',ytit='Orientation (deg, N-E)',xrange=xrange,yrange=yrange
errplot_x,rr,s1,s2

!p.thick=1.
!x.style=0
!p.multi=0

return
end