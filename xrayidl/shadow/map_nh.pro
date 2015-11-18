pro map_nh,sel,nhd,npb,ba,et,nhdnew,xflux,nhback=nhback, $
fitband=fitband,chiband=chiband,maxint=maxint,opfile=opfile, $
nval=nval,chimin=chimin,xfluxfix=xfluxfix,nhdecmin=nhdecmin,nhdecint=nhdec $
,wclimit=wclimit
;-
; using iteration method to get the absorption column density distribution 
; from x-ray data
;
; Basic features: The iteration converges to a minmum chi^2 value where
; no single pixel nhd value changes during the last iteraction. The accuracy
; of the fit is determined by nhdec (when = 0.05 5% accuracy or 5x10^18
; if it is larger).
; 
;**INPUTS:
; nhd == estimated column density image (in units of 10^22 cm^-2). Most of 
; 	pixels should have nhd > 0.;
; npb, ba, et == Containing count, background, and exposure images in
;       different energy bands. e.g., npb= array(60,60,3) includes images
;    	in three bands: 0=S, 1=M, and 2=H;
; fitband - vector containing bands to be used in the least fit
; chiband - vector containing bands to be used in calculating chi^2
; nval - number of NH values to be calculated in an iteration (has to be 
;	an odd value). Def nval=7. Set nval=1 to get a chi^2 value for an
; 	assumed nhd
; xfluxfix - xflux values (in one band) not used in the current fit, 
; 	but used in the chi^2 calculation
; nhband - a constand nh to be subtracted from the input nhd image before
;	the fit
; maxint - maximum number of interation allowed (def = 50)
; opfile - the head name (plus directory if not current, e.g.,
; 	opfile='~wqd/rosat/shadow/phoct_dat.normal.' ) of the opacity file
; nhdecmin - the minimum nhdec for stoping the iteration. The accuracy of
;  	the output map is then nhdecmin*100%*(nh > 10^20). e.g., 
;	if nhdecmin=0.01, the accuracy would be 1% or 10^18 whichever is larger
; 
;**OUTPUTS:
; nhdnew - The column density image (in units of 10^22 cm^-2) constructed from
; the X-ray data. Negative pixel values may indicate local excess emission 
; above the assumed uniform X-ray background;
; xflux - minimum xflux values from the fit
; chimin - the minimum chi^2 value reached
;
;**EXAMPLES:
; map_nh,sel,ir4*0.01,npb,ba,et,nha,xflux,nhdecmin=0.01
;
;**SUBROUTINES CALLED:
; sh_opac
; fit_ab
; min_image
; sel_data
;
;**MODIFICATION HISTORY:
; an updated version of the procedure. By wqd Oct 20, 1993 
;+
; 	pixels should have nhd > 0.;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - map_nh,sel,nhd,npb,ba,et,nhdnew,xflux,'
print,'nhback=nhback,fitband=fitband,chiband=chiband,maxint=maxint'
print,',opfile=opfile,nval=nval,chimin=chimin,xfluxfix=xfluxfix,nhdecmin=nhdecmin,nhdecint=nhdec,wclimit=wclimit'
return
endif
;if n_elements(opfile) eq 0 then opfile='~wqd/rosat/shadow/phoct_dat.normal.'
if n_elements(nhback) eq 0 then nhback=0.
if n_elements(fitband) eq 0 then fitband=[0,1,2]
n_fitband=n_elements(fitband)
if n_elements(chiband) eq 0 then chiband=fitband
n_chiband=n_elements(chiband)
; find out the index in xflux where the values are fixed
if n_elements(xfluxfix) ne 0 then begin
	match,chiband,fitband,indmatch
	fixind=indgen(n_chiband)
	remove,indmatch,fixind
	fixind=fixind(0) ;becomes a scalor
endif
if n_elements(maxint) eq 0 then maxint=50
nhdnew=nhd*0.
nhdnew(sel)=nhd(sel)-nhback

chio=1.e22
onbig=1000000
if n_elements(nhdecmin) eq 0 then nhdecmin=0.01
if n_elements(nhdec) eq 0 then nhdec=0.2 ;10\% as an initial nhd step
nsel=n_elements(sel)
ndf=nsel*(n_chiband-1)-n_fitband*2 ;number degrees of freedom
chilimit=ndf
if n_elements(nval) eq 0 then nval=5 
hnval=nval/2
nhdim=fltarr(nsel,nval)
chibint=fltarr(nsel,nval)

; start the interation 
for i=0,maxint-1 do begin
	; first to get an estimate of xflux from the current estimated nhd
	fit_ab,nhdnew,npb,ba,et,1.,0.,1,xflux,minnorm,fitband=fitband $
	,sel=sel,/insel,opfile=opfile,wclimit=wclimit
	cc=where(xflux lt 0.,nneg)
	if n_elements(xfluxfix) ne 0 then xflux(2*fixind:2*fixind+1)=xfluxfix
	xflux =xflux > 0. ;this may lead to nonmonitonic converge!
	for k=0,nval-1 do begin
	  nhdim(*,k)=nhdnew(sel)+(nhdnew(sel) > 0.01)*nhdec*(k-hnval) 
	  ;negative value of nhdim can be achieved. The accuracy is nhdec%
	  chibin=fltarr(nsel)
	  for n = 0, n_chiband-1 do begin
		  nband=chiband(n)
		  sh_opac,nhdim(*,k),opacity,nband $
			,opfile=opfile,/nolog
		  sel_data,npb,ba,et,sel,nband,count,back,time
		  yfit=back+(xflux(0,nband)+xflux(1,nband)*opacity)*time
		  chibin=chibin+(count-yfit)*(count-yfit)/(count > wclimit)
	  endfor
	  chibint(0:*,k)=chibin
	endfor
        ;replace values in the nhdnew with values resulting smallest chi^2
	; available
   	min_image,chibint,minloc,minchibin 
   	nhdnew(sel)=nhdim(lindgen(nsel),minloc)
   	chimin=total(minchibin)
	cc=where(abs(minloc-hnval) gt 1,nbig) ;nbig - the number of pixels that
					      ;have changes larger than 1 step
   	print,'chimin,ndf,i = ',chimin,ndf,i
   	if (chio-chimin lt 1.e-4*chimin and nbig eq onbig) then begin
		if nhdec gt nhdecmin then begin
			nhdec=nhdec*0.3 
			onbig=1000000
		endif else goto,done
	endif else onbig=nbig
   	chio=chimin
   	print,'nhdec, nbig = ',nhdec,nbig
endfor

done:
nhdnew(sel)=nhdnew(sel)+nhback
end

  
