pro conf_bts_ell_plot,pell,pm,rlo,rhi,rn
;+
; NAME:
;	CONF_BTS_ELL_PLOT
;
; PURPOSE:
;       plot the output parameters from CONF_BTS_ELL: x, y offsets,
;       ellipse, and orientation angle as a function of semi-major axis
;
; CALLING SEQUENCE:
;       CONF_BTS_ELL_PLOT,pell,pm,rlo,rhi,rn
;
; INPUTS:
; pell - best fit parameters from ELL_PLOT_IT
; pm - fit parameters from bootstrapping
; rlo,rhi,rn - the lower and upper of semi-major axis values and the 
;             number of ellipses
;
; OPTIONAL INPUTS:
; none
;	
; KEYWORD PARAMETERS:
; none
;
; OUTPUTS:
; four-panel plots
;
; MODIFICATION HISTORY:
; 	Written by:	WQD,  Aug. 27, 2003
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - CONF_BTS_ELL_PLOT,pell,pm,rlo,rhi,rn'
return
endif

xys=[0,0,0]
r1=0 & r2=rn-1
rr=(rlo+reverse(findgen(rn))*(rhi-rlo)/(rn-1))

!p.charsize=1.2
!p.thick=1.5
!p.multi=[0,2,2,0,0]
!x.style=1
k=0
xrange=[-0.5+min(rr),max(rr)+0.5]
yrange=[min(0.5*(pm(k,r1:r2,1)-xys(k))), max(0.5*(pm(k,r1:r2,2)-xys(k)))]
yt=['R.A. shift (arcsec)','Dec. shift (arcsec)','Ellipticity']
plot,rr,0.5*(pell(k,r1:r2)-xys(k)),psym=6,xtitle='Semi-major axis (arcmin)',ytit=yt(k),xrange=xrange,yrange=yrange
errplot_x,rr,0.5*(pm(k,r1:r2,1)-xys(k)),0.5*(pm(k,r1:r2,2)-xys(k))

k=1
yrange=[min(0.5*(pm(k,r1:r2,1)-xys(k))), max(0.5*(pm(k,r1:r2,2)-xys(k)))]
plot,rr,0.5*(pell(k,r1:r2)-xys(k)),psym=6,xtitle='Semi-major axis (arcmin)',ytit=yt(k),xrange=xrange,yrange=yrange
errplot_x,rr,0.5*(pm(k,r1:r2,1)-xys(k)),0.5*(pm(k,r1:r2,2)-xys(k))

k=2
yrange=[min((pm(k,r1:r2,1)-xys(k))), max((pm(k,r1:r2,2)-xys(k)))]
plot,rr,pell(k,r1:r2)-xys(k),psym=6,xtitle='Semi-major axis (arcmin)',ytit=yt(k),xrange=xrange,yrange=yrange
s=pm(k,r1:r2,1)
elmin=0.05
if min(s) lt elmin then s(where(s lt elmin))=0.
errplot_x,rr,s-xys(k),pm(k,r1:r2,2)-xys(k)

k=3
yrange=[min((pm(k,r1:r2,1))), max((pm(k,r1:r2,2)))]*180./!pi+90.
plot,rr,pell(3,r1:r2,0)*180./!pi+90.,psym=6,xtit='Semi-major axis (arcmin)',ytit='Orientation (deg, N-E)',xrange=xrange,yrange=yrange
errplot_x,rr,pm(3,r1:r2,1)*180./!pi+90.,pm(3,r1:r2,2)*180./!pi+90.

!p.thick=1.
!x.style=0
!p.multi=0
return
end
