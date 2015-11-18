pro source_apm,cra,cdec,infile,outfile=outfile,slow=slow,flow=flow $
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
if n_elements(self) eq 0 then self=1
source_info,souno,sra,sdec,sigma,cntr,sxp,syp,self=self,soufile=soufile $
,slow=slow,flow=flow ;,/pix

if n_elements(infile) eq 0 then infile='cosmos.dat'
openr,un,infile,/get_lun
n_source=10000
para=strarr(n_source)
ra=strarr(n_source)
dec=strarr(n_source)
parac=''
k=0
while not EOF(un) do begin
    	readf,un,parac,form='(a78)'
	stringad,strmid(parac,0,30),rac,decc
	ra(k)=rac
	dec(k)=decc
	para(k)=parac
	k=k+1
endwhile
n_source=k
ra=ra(0:k-1)
dec=dec(0:k-1)
para=para(0:k-1)
trans_dist,cra,cdec,ra,dec,xp,yp,/deg,/das

sra=sra-255.5
sdec=sdec-255.5
dis=sqrt(sra^2+sdec^2)*0.25

if n_elements(radius) ne 0 then begin
	sel=where(dis gt radius,nsel)
	if nsel ne 0 then remove,sel,souno,sxp,syp,sra,sdec
endif

ns=n_elements(sxp)
if keyword_set(vradius) ne 0 then begin
	; get a rough estimate of error circle for each source
	blow=4 & bhigh=5
	detect_params,dis,core,blow=blow,bhigh=bhigh
	core=(sradius*core*120.)^2
endif else core=replicate((sradius*60.)^2,ns)

sxp=(sxp-7680.)*0.5
syp=(syp-7680.)*0.5

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
		print,'k, source = ',k,souno(k)
		forprint,para(sel),sqrt(sdis(sel)),xdis(sel),ydis(sel)
		if nout ne 0 then begin
			for n=0,nsel-1 do printf,un,para(sel(n)) $
			,sqrt(sdis(sel(n))),xdis(sel(n)),ydis(sel(n)),souno(k) $
			,form='(a78,3f7.1,I4)'
		endif
	endif
endfor
if nout ne 0 then free_lun,un
stop
end