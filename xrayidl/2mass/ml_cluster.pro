;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro ml_cluster,sra,sdec,cra,cdec,image_t,image_b,pixsize,rc=rc $
 threshold=threshold,outfile=outfile,chn=chn,sra=sra,sdec=sdec,scntr=scntr
;-
;
; writen by wqd, april 16, 1996
;+
if n_params() eq 0 then begin
print,'ml_anal,list,cra,cdec,image_t,image_b,tblock=tblock,'
print,' radius=radius,threshold=threshold,infile=infile,outfile=outfile'
print,' ,append=append,blow=blow,bhigh=bhigh,sfrac=sfrac,chn=chn'
print,' ,slow=slow,flow=flow,sigma=sigma,rc=rc,verb=verb'
return
endif
;
if n_elements(threshold) eq 0 then threshold=0.
if n_elements(chn) eq 0 then chn=1
;
if n_elements(outfile) eq 0 then outfile=infile+'_ml' 
openw,un,outfile,/get_lun 
trans_dist,cra,cdec,sra,sdec,xp,yp,/deg
if n_elements(rc) ne 0 then rs=rs*0.+rc*120. ;in units of pixels
sigma=rs*0.5
sz=size(image_t)
trans_dist,cra,cdec,sra,sdec,xp,yp,/deg,pixsize=pixsize
xo=(sz(1)-1.)*0.5+xp
yo=(sz(2)-1.)*0.5+yp
;
;get exposures and background counts at the source positions

loc=long(yp+sz(2)*0.5)*sz(1)+long(xp+sz(1)*0.5)
if n_elements(image_t) ne 0 then expt=image_t(loc) $
 else expt=loc*0.+exptime

;----------------------------------
; loop over individual sources
nbin_s=rs^2*!pi 
bc=image_b(loc)*(nbin_s/float(tblock)^2) 
if chn eq 2 then np=3 else np=2
sse=fltarr(ns,3)
csv=fltarr(ns)
sz=csv
gcntr=csv
dsv=csv
status=0
xx=list.x & yy=list.y
if n_elements(maxni) eq 0 then maxni=100
k=0
for kk=0L,(ns-1) do begin 
  xoo=xo(kk) & yoo=yo(kk)
  gsize=sigma(kk)
  for ni=1,maxni do begin
	;get the counts within the source aperture
	sel=where(((xx-xoo)^2+(yy-yoo)^2) le rs(kk)^2,cs)
	if cs gt bc(k) then $		
	   	mle_s,list(sel),bc(kk),rs(kk),para,status=status $
		,sigma=gsize,chn=chn,xo=xoo,yo=yoo,verb=verb,ffdd=ffdd $
	else begin
		status=0
		print,'cs < bc',cs,bc(k),k
	endelse
	if status ne 0 then begin
		status=0
		;k=k-1
		;goto,next
		para=[xoo,yoo,0.]
	endif
	if sqrt((para(0)-xoo)^2+(para(1)-yoo)^2) lt 1. or cs eq 0 or ni eq maxni then begin
	 	;if cs =0, the source will be removed in the output 
	 	csv(k)=cs 
	 	dsv(k)=sqrt((para(0)-xo(kk))^2+(para(1)-yo(kk))^2)
	 	xo(k)=para(0) & yo(k)=para(1)
	 	if chn ge 2 then begin
			sz(k)=para(2)
			if chn eq 3 then gcntr(k)=para(3)
	 	endif
		for kkk =0, np-1 do sse(k,kkk)=ffdd(kkk,kkk)
	 	goto,next
	endif else begin
	 	xoo=para(0) & yoo=para(1)
	 	if chn ge 2 then begin
			gsize=para(2) 
	 	endif
	endelse
  endfor
  print,'ni is greater than maxni'
  next:
  k=k+1
  print,'source No = ',k
if !debug eq 2 then stop
endfor
ns=k
get_snr,csv,nbin_s,bc,nbin_s,csn,snr,sfrac=sfrac,/bmap ;nbin_s is not used

;print,nbin_s,bc,csv,csn,snr,sfrac
cntr=csn/expt
if chn eq 3 then gcntr=gcntr/(expt*gfrac)

;get position in a standard SASS image
ra_dist = xo - hdim
dec_dist =yo - hdim
dis=sqrt(ra_dist^2+dec_dist^2)*(!size_pixel/60.)
trans_loct,ra_dist,dec_dist,cra,cdec,star_ra,star_dec,/deg
trans_degree,star_ra,star_dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg
sse=sqrt(sse) ; in units of pixels
;record these source into output file:
kk=0
for k=0,(ns-1) do begin
	if snr(k) ge threshold then begin
	kk=kk+1
 	print, kk, ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k),snr(k),cntr(k),scntr(k), $
 	dsv(k),dis(k),sse(k,0),sse(k,1),rs(k),sigma(k),sse(k,2) $
	,sz(k)-sigma(k),gcntr(k), $
		format='(I3, 2(2i4, f7.2), f9.2,2f9.5,1x,8f8.2,f9.5)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, kk,' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',snr(k), $
   	  ' |',cntr(k),' |',scntr(k),' |', dsv(k),' |', dis(k),' |' $
		,sse(k,0),' |',sse(k,1),' |',rs(k),' |',sigma(k),' |' $
		,sse(k,2),' |',sz(k)-sigma(k),' |',gcntr(k),' |'  $
	  ,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, 2(f9.5,a2),8(f8.2,a2),f9.5,a2)'
	endif
endfor
free_lun,un
return
end
;================================
pro mle_s,xp,yp,backc,re,para,xo=xo,yo=yo,sigma=sigma,chn=chn $
,verb=verb,ffdd=ffdd,status=status
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - mle_s,list,backc,re,para,xo=xo,yo=yo,sigma=sigma'
print,',chn=chn,verb=verb,ffdd=ffdd,status=status'
return
endif
common data,ii,jj,modelc
; 
if n_elements(sigma) eq 0 then begin 
	if !instr eq 'p' then sigma=26. else sigma=6. ;unites of pixels
endif
ii=xp
jj=yp
tc=n_elements(ii)
if n_elements(chn) eq 0 then choice=2 else choice=chn
if n_elements(xo) eq 0 then xo=median(ii) ;initial position
if n_elements(yo) eq 0 then yo=median(jj)
;--------------------------------------
scale=sigma
ii=(ii-xo)/scale
jj=(jj-yo)/scale
res=re/scale
case choice of
 1: begin
  	modelc=float([choice,tc,backc,res,sigma/scale])
	para=[0,0]
    end
 2: begin
  	modelc=float([choice,tc,backc,res])
	para=[0,0,1.]
    end
endcase
ptol=1.e-4
;if keyword_set(verb) ne 0 then print,'po =',p
proname='scoring_gaussian'
scoring,para,proname,fmin,ffdd=ffdd,status=status

para=para*scale
para(0)=para(0)+xo
para(1)=para(1)+yo
ffdd=ffdd*scale^2
if keyword_set(verb) ne 0 then print,'para =',para
if !debug eq 1 then stop,'stop at the end of MLE'
return
end
;=============================================================
pro scoring,para,proname,fmin,ffdd=ffdd,itmax=itmax,double=double,ptol=ptol $
,stpmax=stpmax,status=status
;+
; Minimization with the scoring method and backtracking algorithm
; para - as input, initial parameter values; as output, parameter values
; 	which give the minimum function 
; proname - the procedure name for evaluating function value, derivatives,
;	and information matrix.
; fmin - output minimum function value
; double - if set, double precision for inverting the information matrix
; stpmax - the maximum step (def stpmax=0.2).
; ptol - parameter convergence requirement: the maximum fraction of change
;	of the parameter values in an iteration (def=10^-7).
; ffdd - the inverse of the information matrix.
;
;*CAUTION:
; para should be carefully normalized to have their values of order unit;
; otherwise, the stpmax needs to be selected carefully.
;
; implemented according to the methods described by C. Sarazin (1980, ApJ
; 236, 75) and by Press et al. (Numerical Recipes, section 9.7).
;
; written by wqd, 9/8/96
;-
if n_elements(stpmax) eq 0 then stpmax=0.2 
if n_elements(itmax) eq 0 then itmax=500
if n_elements(double) eq 0 then double=0
for k=1,itmax do begin
	parao=para
	call_procedure,proname,parao,ff,ffd,ffdd
	ffdd=nr_invert(ffdd,status,/double)
	if status eq 1 then begin
		print,'singular array: ffdd inverse'
		return
	endif
	if status eq 2 then begin
		print,'small pivot element: ffdd inverse'
		return
	endif		
	dpara=-ffd##ffdd
;	nr_ludcmp,ffdd,ind
;	dpara=-nr_lubksb(ffdd,ind,ffd)
	lnsrch,parao,ff,ffd,dpara,para,fmin,stpmax,check,proname,tolx=ptol 
		;backtracking
	if check eq 1 then return
;	print,'para,ff,ffd,ffdd,fmin ',para,ff,ffd,ffdd,fmin
	if !debug eq 1 then stop,'stop inside scoring loop'
endfor
print,'maximum itmax exceeded'
return
end
;=============================================================
pro scoring_gaussian,xx,ff,ffd,ffdd
common data,ii,jj,modelc
na=n_params() 
ffdk=fltarr(modelc(1),n_elements(xx))
case modelc(0) of 
  1: begin
	ssigma=(modelc(n_elements(modelc)-1))^2
	rcs=modelc(n_elements(modelc)-2)^2
	areac=1.-exp(-rcs/(2.*ssigma))
	iid=ii-xx(0)
	jjd=jj-xx(1)
	e=(iid^2+jjd^2)/(2.*ssigma)
	nb=modelc(2)/modelc(1)
	psf=(1.-nb)*exp(-e)/(2.*!pi*ssigma)/areac
	rr=nb/(!pi*rcs)+psf
	ff=-total(alog(rr))
	rr=psf/rr

	if na gt 2 then begin ;getting derivatives and information matrix
	 ffdk(*,0)=rr*iid/ssigma
	 ffdk(*,1)=rr*jjd/ssigma
	 ffdk=-ffdk
	 ffd=total(ffdk,1)
	 ffdd=ffdk##transpose(ffdk)
	endif
     end
  2: begin
	ssigma=(xx(2))^2
	rcs=modelc(n_elements(modelc)-1)^2
	areac=1.-exp(-rcs/(2.*ssigma))
	iid=ii-xx(0)
	jjd=jj-xx(1)
	e=(iid^2+jjd^2)/(2.*ssigma)
	nb=modelc(2)/modelc(1)
	psf=(1.-nb)*exp(-e)/(2.*!pi*ssigma)/areac
	rr=nb/(!pi*rcs)+psf
	ff=-total(alog(rr))
	rr=psf/rr

	if na gt 2 then begin ;getting derivatives and information matrix
	 ffdk(*,0)=rr*iid/ssigma
	 ffdk(*,1)=rr*jjd/ssigma
	 ffdk(*,2)=((1.-areac)*rcs/(2.*ssigma)/areac-2.+2*e)*rr/xx(2)
	 ffdk=-ffdk
	 ffd=total(ffdk,1)
	 ffdd=ffdk##transpose(ffdk)
	endif
     end
endcase
if !debug eq 1 then stop
return
end