pro get_filter,filter,filter_sub,xs_pix,ys_pix,block=block,sra=sra,sdec=sdec $
,radius=radius,cra=cra,cdec=cdec,slow=slow,flow=flow,subvalue=subvalue $
,blow=blow,bhigh=bhigh,factor=factor,pixcenter=pixcenter,soufile=soufile $
,rout=rout,rin=rin,cntrlimit=cntrlimit,perclimit=perclimit,psffile=psffile
;
;*PARAMETERS:
; INPUTS:
;       filter - input filter centered on xs_pix and ys_pix
;	xs_pix,ys_pix - optional inputs (with default at the center of
;	the image). the x and y norminal (0.5") pixel coordinate of 
;	the filter center. The reference pixel could be either at the
;	center (then pixcenter needs to be set) or relative to the lower
;	left corner pixel (1,1) as appears in the sass hardcopy.
;	radius - source subtraction radius in units of filter pixels
;	cra,cdec - RA and DEC of the observation (degree). If not given
;	they will be determined using files in the current directory.
;	sra,sdec - RA and DEC of the sources (degree). if not given
;		soufile will be used.
; 	blow, bhigh - lower and upper boundaries of the PSPC bands used
;		to construct the images (for PSPC observations only)
;    	block - the size of the filter pixel (in units of 0''.5 ), default=1
;	factor - the fraction of the default radii in source subtractions
;		for PSPC observations only.
;	subvalue - the value used to give in the image regions where sources
;		are subtracted (def = 0.). A non-zero value may be used
;		to identify regions of the sources.
;	deg - if set, the input sra and sdec etc are assumed to be in units
;		of degrees
; OUTPUTS
;	filter_sub
;
;*PROCEDURE:
;	finds the locations of the sources in the image in pixels and
;	the regions which are contaminated by the sources, and sets
; 	the values of the regions into zero (or subvalue).
;
;*EXAMPLES:
;	get_filter,4694.2,3582.5,fltarr(100,100)+1.,ts,block=16,radius=5
;*RESTRICTIONS:
;
;*SUBROUTINES CALLED:
;	source_info
;	source_sub_v
;
;*MODIFICATION HISTORY:
; writen by wqd, Dec. 9, 1994
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - get_filter,filter,filter_sub,xs_pix,ys_pix,block=block'
print,',sra=sra,sdec=sdec,radius=radius,cra=cra,cdec=cdec,slow=slow,flow=flow'
print,',subvalue=subvalue,blow=blow,bhigh=bhigh,factor=factor'
print,',pixcenter=pixcenter,soufile=soufile,rout=rout,rin=rin'
print,',cntrlimit=cntrlimit,perclimit=perclimit,psffile=psffile'
;
return
endif
if n_elements(soufile) eq 0 then soufile=!seq_no+'_sou_sass.dat'
if n_elements(block) eq 0 then block=1.
sz=size(filter)
if n_elements(xs_pix) eq 0 then begin
	xs_pix=0
	ys_pix=0
	pixcenter=1
endif ;assuming the filter's reference center at the pointing direction

if keyword_set(pixcenter) eq 0 then begin
	if !instr eq 'h' then cpix=4096.5
	if !instr eq 'p' then cpix=7680.5
	xp=xs_pix-cpix ;assuming the lower left cornor pixel at (1,1)
	yp=ys_pix-cpix ; for xs_pix, and ys_pix
endif else begin
	xp=xs_pix
	yp=ys_pix
endelse
xcpix=-(xp/block-(sz(1)-1)*0.5)
ycpix=-(yp/block-(sz(2)-1)*0.5)
if n_elements(sra) eq 0 then $
	source_info,sid,sra,sdec,sigma,cntr,soufile=soufile,/deg,slow=slow
if n_elements(cra) eq 0 then image_center,cra,cdec,/deg
if n_elements(rout) ne 0  then begin
	trans_dist,cra,cdec,sra,sdec,sxp,syp,/deg
	sel=where((sxp-xp)^2+(syp-yp)^2 le (rout*60/!size_pixel)^2,nsel)
	if nsel eq 0 then stop,'no source inside the radius rout'
	sra=sra(sel)
	sdec=sdec(sel)
endif
if n_elements(rin) ne 0 then begin
	trans_dist,cra,cdec,sra,sdec,sxp,syp,/deg
	sel=where((sxp-xp)^2+(syp-yp)^2 ge (rin*60/!size_pixel)^2,nsel)
	if nsel eq 0 then stop,'no source outside the radius rin'
	sra=sra(sel)
	sdec=sdec(sel)
endif
filter_sub=filter
filter_sub=source_sub_v(filter,cra,cdec,sra,sdec,cntr,block=block $
	,xcpix=xcpix,ycpix=ycpix,sradius=radius,/deg,cntrlimit=cntrlimit $
	,perclimit=perclimit,psffile=psffile $
          ,blow=blow,bhigh=bhigh,factor=factor,subvalue=subvalue)
return
end
