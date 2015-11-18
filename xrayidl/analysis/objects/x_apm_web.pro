pro x_apm_web,cra,cdec,infile,outfile=outfile,slow=slow,flow=flow $
 ,soufile=soufile,sradius=sradius,self=self,vradius=vradius,radius=radius
;-
; Find out apm objects within a certain range of X-ray sources
; or in individual bands
; cra,cdec - center of a referece pixel (eg. the image center)
; infile - input file name containing a list of the image seq numbers and 
; 	will not be used if soufile is given
; soufile - a vector containing the source file names (including directories
;	relative to the current directory
; slow, flow - S/N and flux selection criteria for the sources to be merged
; sradius - the maximum core radius in which sources are considered to 
;	be duplicate ones (in arcmin). Def sradius=0.5
; self - use the *_src.fits soufile format
; vradius - if set, the sradius defines as the on-axis 90\% radius
;	the actual matching radius will vary will off-axis radius
; radius - the radius (arcmin) for selecting sources
;*outputs:
; outfile - output file name to contain the matched source and object list
;*example
; writen by WQD, June 17, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_apm,cra,cdec,infile,outfile'
print,',slow=slow,flow=flow,soufile=soufile,sradius=sradius'
print,',self=self,vradius=vradius,radius=radius'
return
endif
if n_elements(sradius) eq 0 then sradius=0.5
if n_elements(self) eq 0 then self=0

openr,un,soufile,/get
ns=1000
souno=lindgen(ns)
sra=fltarr(ns)
sdec=fltarr(ns)
stext=strarr(ns)
str=''
k=0
while not (eof(un)) do begin
	readf,un,str,format='(a63)
	souno(k)=fix(strmid(str,0,8))
	stringad,strmid(str,38,24),rascale,decscale
	sra(k)=rascale
	sdec(k)=decscale
	stext(k)=str
	k=k+1
endwhile
free_lun,un
souno=souno(0:k-1)
sra=sra(0:k-1)
sdec=sdec(0:k-1)
stext=stext(0:k-1)

trans_dist,cra,cdec,sra,sdec,sxp,syp,/deg,/das
dis=sqrt(sxp^2+syp^2)/60.
if n_elements(radius) ne 0 then begin
	sel=where(dis gt radius,nsel)
	if nsel ne 0 then remove,sel,souno,sxp,syp,stext
endif
ns=n_elements(sxp)

if n_elements(infile) eq 0 then infile='apm_36m.dat'
apm_info,ra,dec,text=para,fname=infile
trans_dist,cra,cdec,ra,dec,xp,yp,/deg,/das

core=replicate((sradius*60.)^2,ns)

nout=0
if n_elements(outfile) ne 0 then begin
	openw,un,outfile,/get_lun
	nout=1
endif
for k=0,ns-1 do begin
	xdis=xp-sxp(k)
	ydis=yp-syp(k)
	sdis=xdis^2+ydis^2
	sel=where(sdis le core(k),nsel)
	if nsel ne 0 then begin
		print,'X-ray source: ', stext(k)
		print,'off target (arcmin) = ',dis(k)
		for n=0,nsel-1 do begin
		 forprint,'APM object: ',para(sel(n))
		 forprint,sqrt(sdis(sel(n))),xdis(sel(n)),ydis(sel(n))
		 if nout ne 0 then printf,un,para(sel(n)) $
		  ,sqrt(sdis(sel(n))),xdis(sel(n)),ydis(sel(n)),souno(n) $
			,form='(a78,3f7.1,I4)'
		endfor
	endif
endfor
if nout ne 0 then free_lun,un
end