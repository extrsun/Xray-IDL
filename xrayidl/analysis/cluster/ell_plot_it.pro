pro ell_plot_it,xp,yp,rlo,rhi,rn,ppa,corner,hdr=hdr,lines=lines $
,color=color,bpix=bpix,plimit=plimit,maxni=maxni,choice=choice,inpar=inpar $
,noverb=noverb,ww=ww 
;+
; NAME:
;	ELL_PLOT_IT
;
; PURPOSE:
;        main program for analysing isophote shapes and orientations of a
;	count distribution
;
; CALLING SEQUENCE:
;       ELL_PLOT_IT,xp,yp,rlo,rhi,rn,ppa,corner
;
; INPUTS:
; xp, yp - xp, yp - arrays containing pixel coordinates of individual counts
; rlo,rhi,rn - lower, upper radius limits, and the number of divisions
;
; OPTIONAL INPUTS:
; inpar - initial parameter values
; cor - plot position from cont_grey (only if hdr is provided)
; hdr - fits header
; bpix - number of background counts per pixel
; plimit - conversion limit (percentage)
; lines, color - standard plot keywords
; noverb - if set, minimum outputs will be given
; ww - 	weight used by ELL_IT
; maxni - maximum allowed number of iteration in the fit 
;
; OUTPUTS:
; ppa - array contain the parameters
;
; SIDE EFFECTS:
;	Describe "side effects" here.  There aren't any?  Well, just delete
;	this entry.
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by:	WQD,   9/29/96
;       Major revision by WQD, Aug, 27, 2003, removing keywords
;       xc,yc,xref,yref, etc.
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - ell_plot_it,xp,yp,rlo,rhi,rn,pv,corner,hdr=hdr'
print,'lines=lines,color=color,bpix=bpix,plimit=plimit,maxni=maxni'
print,',inpar=inpar,noverb=noverb,ww=ww' 
return
endif

rv=rhi-(rhi-rlo)*findgen(rn)/(rn-1.)
for k=0,rn-1 do begin
	ell_it,xp,yp,pv,radius=rv(k),plimit=plimit,bpix=bpix,maxni=maxni $
		,choice=choice,inpar=inpar,noverb=noverb,ww=ww
        if n_elements(hdr) ne 0 then $
        ell_plot,pv,rv(k),corner,hdr,lines=lines,color=color
                ;!size_pixel is used in ell_plot
	if k eq 0 then ppa=fltarr(n_elements(pv),rn)
	ppa(*,k)=pv
	inpar=pv
	inpar(2)=0. ; for next radius
endfor
print,'ppa = '
print,ppa
return
end
