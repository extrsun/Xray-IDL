pro psf_frac,offcenv,group,rate,radius,binrat=binrat $
,perclimit=perclimit,outfile=outfile
;-
; calculate the radius that contains a percentage (def = 90%) of
; expected counts of a
; point-like source as a function of the off-axis angle of the PSPC or
;	the HRI (to be tested)
; add the function to get the weighted sigma of the PSPC psf's Gaussian
; component
;
;
;*INPUTS:
; nbins - Number of bins of the output image of psf (used by calcpsfp.pro)
; binsize - Size of bin (in arcsec) for the on-axis psf, the binsize is
;		scaled linearly up for off-axis angles.
; group - pha channel boundaries (that can be got from modelrate.pro)
; offcenv - vector containing off axis angles of assumed source positions
;	(in units of arcmin) where the radius is going to be evaluated
; rate - vector containing observed (or assumed) counting rate for
;	each channel boundaries (produced by modelrate.pro)
; binrat - ratio between unmber of output bins and number of pixels for
;	calculation (default=11)
; perclimit - vector containing percentages for calculating the
; radii as the function of off-axis angles (def = [0.5,0.7,0.8,0.85,0.9,0.95])
; 	The radius of 0.85 or below is well defined. Around 0.9
;	the psf starts to be dominated by the Loretz factor
;
;*OUTPUTS:
; radius - the percentage radius (in units of arcmin)
;		as a function of the off-axis angles
; outfile - the output file name, if provided, the values of offcenv and
;	 radius will be written into a file in '~/rosatdata/source/'
;	The file is to be read by detect_params.pro
;
;*EXAMPLE:
;	modelrate,'~/spectrum/pw_2_0.03.dat',12,29,rate,group
; psf_frac,findgen(22)*3.,group,rate,r,outf='~/rosatdata/source/psf_pw_2_0.03_47.dat'
;
;
; written by WQD, Sept 11, 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - psf_frac,nbins,binsize,group,offcenv,rate,radius, $
print,'	binrat=binrat,perclimit=perclimit'
return
endif
;
common share,offang,prof,prof1
nbins=61
binsize=10
binrat=3
if n_elements(perclimit) eq 0 then perclimit=[0.5,0.7,0.8,0.85,0.9,0.95]
na=n_elements(offcenv)
sigmav=fltarr(na)
rrate=fltarr(na)
nl=n_elements(perclimit)
radius=fltarr(na,nl)

fout=1.-perclimit
maxfout=max(fout)

for k=0, na-1 do begin
	psfprof,offcenv(k),rate,offang,prof,/defang $
		,group=group,sigmaw=sigma,prof1=prof1

	; total rate contained in the prof
	noff=n_elements(offang)
	trate=prof(0)*!pi*offang(1)^2+simpson('get_intpsf' $
		,offang(1),offang(noff-1))
	trate1=prof(0)*!pi*offang(1)^2+simpson('get_intprof1' $
		,offang(1),offang(noff-1))
	;print,'trate,trate1 =',trate,trate1
	prof=prof/trate ;renormalize the profiles
	prof1=prof1/trate
	; There may be some errors in psfprof, prof is not completely
	; normalized. But Hasinger's formula are too complicated to
	; to make a good check.

	frac=fltarr(noff)
	kk=noff-1
	frac(kk)=0.
	while frac(kk) lt maxfout do begin
	  kk=kk-1
	  frac(kk)=frac(kk+1)+simpson('get_intpsf',offang(kk),offang(kk+1))
	endwhile
	linterp,frac(kk:*),offang(kk:*),fout,offsalimit
	radius(k,*)=offsalimit
	rrate(k)=trate1/trate
	sigmav(k)=sigma
	print,'k, offcen,radius = ',k, offcenv(k),radius(k,*),trate1/trate
endfor
radius=radius/60.
if n_elements(outfile) ne 0 then begin
	openw,unout,outfile,/get_lun
	printf,unout,na,nl
	printf,unout,perclimit
	printf,unout,offcenv,radius,sigmav,rrate
	free_lun,unout
endif
return
end
;=================================================
function get_intpsf,angle
common share,offang,prof,prof1
linterp,offang,prof,angle,intpsf
;quadterp,offang,prof,angle,intpsf ;makes no significant difference
intpsf=intpsf*!pi*2.*angle
return,intpsf
end
;=================================================
function get_intprof1,angle
common share,offang,prof,prof1
linterp,offang,prof1,angle,intprof1
intprof1=intprof1*!pi*2.*angle
return,intprof1
end
