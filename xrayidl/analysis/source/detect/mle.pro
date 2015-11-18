pro mle,list,dim,para,imin=imin,jmin=jmin,filter=filter $
 ,block=block,sigma=sigma,chn=chn,cntrfac=cntrfac,xo=xo,yo=yo,xc=xc,yc=yc $
,image_t=image_t
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;-
;*PURPOSE:
; Conduct Maximum Likelihood fitting of count distribution 
;
;*CALLING SEQUENCE:
; mle,list,dim,para,imin=imin,jmin=jmin,filter=filter 
;	,block=block,sigma=sigma,chn=chn,cntrfac=cntrfac,xo=xo,yo=yo
;
;*PARAMETERS:
; INPUTS:
;  list - count list
;  imin,jmin - the lower left corner pixel coordinates in the image
; 		if not given, xc and yc (or their defaults) will be used
;  dim - image dimension in units of 0.5"
; filter - a exposure map which also used as a filter for selecting 
;	 source and background regions: < 0 - source region; > 0 background
;	region; =0 - excluded region (e.g., nearby sources)
; 	def - all pixels used in the fit and background = 0
; block - the block used for filter
; sigma - the size of gaussian used in the fit (def = 13 for PSPC and
;		= 3 for HRI arcsecas)
; chn - if 2, four parameters, cntr and sigma as well as y positions are
;		fitted 
; cntrfac - correction factor for cntr (def =1)
; xo, yo - initial source pixel position (e.g., SASS pixel position)
;	def = median position of counts in the source region
; xc, yc - the pixel position of the image
;*OUTPUTS:
;  para - vector containing cntr,cntre, and fitting parameters
;
;*PROCEDURE:
; call nr_powell is used for the fitting
;
;*EXAMPLES:
; mle,l,200,fil=fs,inf='test.dat',xc=4096.5+xp,yc=4596.5+yp
;*RESTRICTIONS:
; works only for well defined sources, nr_powell could fail to converge
;
;*MODIFICATION HISTORY:
; writen by WQD, Mar 8, 1995
; add keywords xc and yc and change imin and jmin into keywords 
; WQD, Mar 13, 1995
;+
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - mle,list,dim,para,imin=imin,jmin=jmin,filter=filter'
print,',filter=filter,block=block,sigma=sigma,chn=chn,cntrfac=cntrfac'
print,',xo=xo,yo=yo,xc=xc,yc=yc'
return
endif
common powell,ts,cbn,csn,ssigma,i,j,choice
; 
if n_elements(block) eq 0 then block=1
if n_elements(sigma) eq 0 then begin 
	if !instr eq 'p' then sigma=13. else sigma=3. ;unites of arcsec
endif
if n_elements(cntrfac) eq 0 then cntrfac=1.
if n_elements(filter) eq 0 then filter=replicate(-1,dim,dim)
; no background region, all pixels are used in the fit
;
if n_elements(chn) eq 0 then choice=2 else choice=chn

list_image_m,list,dim,image,imin,jmin,block=block,sel=sel,loc=loc,filter=filter,xc=xc,yc=yc
list_s=list(sel)

; only retain the source region
sel=where(filter(loc) lt 0,nsel)
if nsel ne 0 then list_s=list_s(sel) else $
	stop,'stop: no source counts in the list'
i=list_s.x
j=list_s.y
;-----------------------------------------
; get exposure, pixel number, total counts
sel=where(filter gt 0,nsel_b)
if nsel_b ne 0 then begin
	cb=total(image(sel)) 
;	tb=nsel_b ;total(image_t(sel))
	tb=total(image_t(sel))
endif else begin
	cb=0.
	tb=0.
endelse

sel=where(filter lt 0,nsel_s) 
if nsel_s ne 0 then begin
	cs=total(image(sel)) 
;	ts=nsel_s ;total(image_t(sel))
	ts=total(image_t(sel))
endif
;----------------------------- 
if tb ne 0 then cbn=cb*ts/float(tb) else cbn=0. 
	;normalized to the source region
csn=cs-cbn ; subtract the background
ssigma=(2.*sigma)^2

if n_elements(xo) eq 0 then xo=median(i) ;initial position
if n_elements(yo) eq 0 then yo=median(j)
ftol=1.e-4
case choice of 
 1: begin
  xi=transpose([[1.,0.],[0.,1.]]) 
  p=[xo,yo]
    end
 2: begin
  xi=transpose([[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.]]) 
  p=[xo,yo,csn,ssigma]
    end
endcase
print,'po = ',p
;initial direction
func='func_powell'
nr_powell,p,xi,ftol,fmin,func,itmax=400
;print,'fmin = ',fmin
cntr=csn*(nsel_s/ts/cntrfac)
cntre=sqrt(cs+cbn*ts/float(tb))*(nsel_s/ts/cntrfac)
print,' p, s/n= ',p,cntr/cntre
para=[cntr,cntre,p]
if !debug eq 1 then stop,'stop at the end of MLE'
return
end
;=============================================================
function func_powell,x
;-
; ssigma - square of the sigma in units of pixel
;+
common powell,ts,cbn,csn,ssigma,i,j,choice
case choice of 
  1: begin
	psf=exp(-((i-x(0))^2+(j-x(1))^2)/(2.*ssigma))/(2.*!pi*ssigma)
	c=total(alog(cbn/ts+csn*psf))
	return,2*(cbn+csn-c)
     end
  2: begin
	psf=exp(-((i-x(0))^2+(j-x(1))^2)/(2.*x(3)))/(2.*!pi*x(3))
	c=total(alog(cbn/ts+x(2)*psf))
	return,2*(cbn+x(2)-c)
     end
endcase
end