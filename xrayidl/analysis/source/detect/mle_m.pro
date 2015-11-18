pro mle,list,xmin,ymin,dim,p,dis,rbp,filter=filter,block=block,sigma=sigma $
,choice=choice
common shared,ac,b,s,ssigma,i,j,choice
; filter - a exposure map which also used as a filter for selecting 
;	 source and background regions: < 0 - source region; > 0 background
;	region; =0 - excluded region (e.g., nearby sources)
; 	The dimension = dim
; 
if n_elements(block) eq 0 then block=1
if n_elements(sigma) eq 0 then begin
	if !instr eq 'p' then sigma=20. else sigma=5.
endif
if n_elements(filter) eq 0 then filter=replicate(-1,dim,dim)
; no background region, all pixels are used in the fit
;
if n_elements(choice) eq 0 then choice=2
list_image,list,xmin,ymin,image,dim,block=block,sel=sel,loc=loc,filter=filter
list_s=list(sel)

; only retain the source region
sel=where(filter(loc) lt 0,nsel)
if nsel ne 0 then list_s=list_s(sel) else stop,'stop: no counts in the list'
i=list_s.x
j=list_s.y
;-----------------------------------------
; get exposure, pixel number, total counts
sel=where(filter gt 0,nsel_b)
if nsel_b ne 0 then begin
	cb=total(image(sel)) 
	tb=total(filter(sel))
endif else begin
	cb=0.
	tb=0.
endelse

sel=where(filter lt 0,nsel_s) 
if nsel_s ne 0 then begin
	cs=total(image(sel)) 
	ts=total(filter(sel))
endif
;----------------------------- 
ac=-float(ts) ;area x exposure
if tb ne 0 then b=cb*ac/float(tb) ;normalized to the source region
s=cs-b ; subtract the background
ssigma=(sigma/block)^2

xo=median(i) ;initial position
yo=median(j)
p=[xo,yo,s,ssigma]
print,'po = ',p
ftol=1.e-4
xi=transpose([[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.]]) 
;initial direction
func='func_powell'
nr_powell,p,xi,ftol,fmin,func
print,' p = ',p
print,'fmin = ',fmin
if n_params() gt 4 then begin
	if choice eq 2 then ssigma=p(p(3))
	if choice eq 2 then cntr=p(2)*nsel_s/ts else cntr=s*nsel_s/ts
	dis=findgen(nint(dim/2.))
	rbp=cb/float(tb)*4.+cntr*4*exp(-dis^2/(2.*!pi*ssigma))/(2.*!pi*ssigma)
	dis=dis*0.5
endif
stop
return
end
function func_powell2,x
;-
; ssigma - square of the sigma in units of pixel
;+
;common shared,ac,b,s,ssigma,i,j,choice
case choice of 
  1: begin
	psf=exp(-((i-x(0))^2+(j-x(1))^2)/(2.*!pi*ssigma))/(2.*!pi*ssigma)
	c=total(alog(b/ac+s*psf))
	return,2*(b+s-c)
     end
  2: begin
	psf=exp(-((i-x(0))^2+(j-x(1))^2)/(2.*!pi*x(3)))/(2.*!pi*x(3))
	c=total(alog(b/ac+x(2)*psf))
	return,2*(b+x(2)-c)
     end
endcase
end