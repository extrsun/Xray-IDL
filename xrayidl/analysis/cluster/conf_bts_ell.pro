pro conf_bts_ell,xp,yp,rlo,rhi,rn,paraa,pam,inparo,lines=lines,bpix=bpix $
,nsim=nsim,seed=seed,siglevel=siglevel,choice=choice,noverb=noverb $
,pradius=pradius
;+
; NAME:
;	CONF_BTS_ELL
;
; PURPOSE:
;      Calculate confidence limits using bootstrapping samples of
;      count data 
;
; CALLING SEQUENCE:
;       CONF_BTS_ELL,xp,yp,rlo,rhi,rn,paraa,pam,inparo
;
; INPUTS:
; xp, yp - arrays containing pixel coordinates of individual counts
; rlo,rhi,rn - the lower and upper of semi-major axis values and the 
;             number of ellipses
; pell - best fit parameters from ELL_PLOT_IT
; pradius - radius used to select counts used in the
;           bootstrapping (in units of pixels) and needs to
;           be greater than the largest major-axis plus any
;           potential offset.
;
; OPTIONAL INPUTS:
; signlevel - one-side confidence limit used by AVG_MEDIAN 
; (e.g., 0.05 = 90% confidence)
; 
; OUTPUTS:
; paraa - array contain fit parameters from bootstrapping for all
;         sampled data
; pam - the median, lower and upper limits of the parameters derived
;       from paraa
;
; SIDE EFFECTS:
;	Fits may not converge for some data set. But the problem is
;	not noticeable.
;
; PROCEDURE:
; First produce a boostrap data samples from the original data and
; then run ell_plot_it to fit the ellipse parameters. Finally
; calculate the lower and upper limits, etc.
;
; MODIFICATION HISTORY:
; 	Written by:	WQD,  7/15, 1995
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - conf_bts_ell,xp,yp,rlo,rhi,rn,paraa,pam,xp,yp'
print,',lines=lines,bpix=bpix,nsim=nsim,seed=seed,siglevel=siglevel'
print,',choice=choice,noverb=noverb,inparo=inparo,pradius=pradius,pbest=pbest'
return
endif

if n_elements(pradius) ne 0 then begin ;select only the counts to be used
    hdim=!pref
    sel=where((xp-(inparo(0)+hdim))^2+(yp-(inparo(1)+hdim))^2 le pradius^2,nc)
    if nc eq 0 then stop,'No count is within the radius!!!'
    xpsel=xp(sel)
    ypsel=yp(sel)
endif else begin
    xpsel=xp
    ypsel=yp
    nc=n_elements(xp)
endelse
bts,seed,lindgen(nc),s,nbs=nsim

for k=0,nsim-1 do begin
 xx=xpsel(s(*,k))
 yy=ypsel(s(*,k))
 inpar=inparo
 ell_plot_it,xx,yy,rlo,rhi,rn,para,cor,bpix=bpix,choice=choice $
	,noverb=noverb,inpar=inpar ;,xc,yc
 if k eq 0 then begin
	sz=size(para)
	paraa=fltarr(sz(1),rn,nsim)
 endif
 paraa(*,*,k)=para
endfor
bts_perc,paraa,pam,siglevel=siglevel,pbest=inparo(3,*)
return
end

