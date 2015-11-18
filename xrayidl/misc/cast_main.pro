pro cast_main,filelist,mhdr,mmap,fdir=fdir,ftail=ftail,mtype=mtype $
,offset=offset,nozero=nozero,avgkey=avgkey,probth=probth $
,slow=slow,flow=flow,soufile=soufile,instr=instr,subfac=subfac,psffile=psffile,eband=eband,mtoi=mtoi,nonorm=nonorm,fname=fname,nointerp=nointerp,tanorm=tanorm
;+
; cast  (exposure) images, possibly from a list of observations, into
; a map. Source regions may be removed from individual observations
; before being merged
;
;*INPUTS:
; filelist - file list: each row contains a file name (or filehead so that
;		file name = filehead+ftail		
; mhdr - the header of the merged map, which can be produced with get_fitshead
;
;*OUTPUTS:
; mmap - merged image, which have the same type as the input image, 
; if avgkey is not set; othwerwise, type=1 (float).
;
;*OPTIONAL Inputs:
; fdir - file directory (def ='../xdata/')
; ftail - characters appended to the filenames in filelist (e.g., '_i')
; mtype - if =1, the merged coordinates are assumed to be Galactic
; offset - if the reference pixel of inhead is not in FORTRAN formation,
;	offset needs to be set (e.g., (e.g., my old IDL format; 
;	1024 for xdim=2048; offset =0)
;	default=1 (FORTRAN format)
; avgkey - if = 1, average will be assigned to the output value, even for
;		integer input. (def: avgkey=1 for float or double input images
;		(type=4 or 5) 
; nozero - if =1, pixels with value=0 will not be casted (def =0)
; soufile - source file name
; probth, slow, flow  - the source selection threshold: ML
;                       probability, signal-to-noise, and/or count
;                       rate, for source subtraction
; instr - instrument name (def = !instr)
; subfac - subtraction factor of the source energy-encircled radius as
;		defined in sou_det_params
; psffile = psf file name. Def = psffile from sou_det_params if eband is not
;		specified, otherwise use the bpsffile(eband-1)
; eband - energy band (1,2,3, 4). 
; mtoi - if set, use cast_r (mutiple pixels merged into one output bin)
;	otherwise use cast
; nonorm - if set, no normalization of the input map (e.g., a
;          background map)
;
; written by wqd 2/22/2001
; revised by wqd (May 25, 2003) to for using updated subroutines
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - cast_main,filelist,mhdr,mmap,fdir=fdir,ftail=ftail'
print,',mtype=mtype,offset=offset,nozero=nozero,avgkey=avgkey,probth=probth'
print,',slow=slow,flow=flow,souf=soufile,instr=instr,subfac=subfac,psffile=psffile,eband=eband,nonorm=nonorm,fname=fname,tanorm=tanorm'
return
endif
if n_elements(mtype) eq 0 then mtype=0
if n_elements(fdir) eq 0 then fdir='../xdata/'
if n_elements(ftail) eq 0 then ftail=''
;-----
if n_elements(eband) eq 0 then eband=''
;sources for subtraction:
if n_elements(soufile) ne 0 then begin
    sou_fits_info,soufile,slist,probth=probth,slow=slow,flow=flow

	;setup the source detection parameters:
	if n_elements(instr) eq 0 then instr=!instr
	sou_det_params,instr,dim,block,ccd,bmin,bmax $
		,dsfrac,asfrac,psffiled,bpsffile ;,ftail,aimoff,subfac
;	if n_elements(psffile) eq 0 then begin
;		if n_elements(eband) ne 0 then psffile=bpsffile(eband-1) $
;		else psffile=psffiled
;	endif

endif
;-------------
if n_elements(fname) eq 0 then begin
    openr,un,filelist,/get
    filename=''
    while not eof(un) do begin
	readf,un,filename
        if n_elements(fname) eq 0 then fname=filename else $
        fname=[fname,filename]
    endwhile
    free_lun,un
endif 
fhead=fdir+fname+ftail
evtfname=fdir+fname+'.fits' ;this event file has to be in the same dir
nfname=fhead+replicate(strtrim(eband,2)+'.fits',n_elements(fname))
append=0
for kk=0,n_elements(nfname)-1 do begin
 if exist(nfname(kk)) then begin
     print,nfname(kk)
    if keyword_set(nonorm) then begin
        inarr=readfits(nfname(kk),inhead)
    endif else begin
	file_params,evtfname(kk),h,cra,cdec,expt
	inhead=headfits(nfname(kk))
	map_exp,expt,inarr,bv=eband,fhead=fhead(kk),tanorm=tanorm
    endelse 
	cra=sxpar(inhead,'crval1')
	cdec=sxpar(inhead,'crval2')
	if n_elements(soufile) ne 0 then $
	inarr=source_sub_v(inarr,cra,cdec,slist.ra,slist.dec,slist.cntrb,block=block $
	,fac=subfac,/deg,perc=asfrac,psffile=psffile) ;,cra=cra,cdec=cdec)
		;assuming the image center is the same as the axis
		;and the default block 
	if keyword_set(mtoi) then begin
		cast_r,mhdr,mmap,inhead,inarr,mtype=mtype $
	 ,offset=offset,nozero=nozero,append=append,avgkey=avgkey 
            endif else begin	
			if mtype eq 1 then gc=3
			cast,'',mhdr,outa=outa,inh=inhead,ina=inarr,gc=gc $
                          ,nointerp=nointerp
			if append eq 1 then mmap=mmap+outa else mmap=outa
	endelse
	append=1
    endif else print,'file ',nfname(kk),' does not exist!!!'
    if !debug eq 3 then stop
endfor
return
end
