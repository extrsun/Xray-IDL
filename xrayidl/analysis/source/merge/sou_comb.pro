pro sou_comb,flist,outfile,fdir=fdir,ftail=ftail,fsdir=fsdir $
	,fstail=fstail,rlo=rlo,rhi=rhi,bflow=bflow,bsoufile=bsoufile $
		,perclimit=perc,psffile=psffile,cntrth=cntrlimit,factor=factor
;+
; Combine sources from a list of observations into a single file
;
;*INPUTS:
; filelist - file list: each row contains an event file name
;
;*OUTPUTS:
; outfile - file name for the output parameters in a LaTex format
;
;*OPTIONAL Inputs:
; fdir - file directory (def ='../xdata/')
; ftail - characters appended to the filenames in filelist (e.g., '.fits')
; fsdir - source file directory, def = ''
; fstail - source file extension to the file names list in flist
;		def = '_map70_mlm_ratio.fits'
;
; running in sou directory: 
; sou_comb,'../imaging/flist.dat','sou_comb.fits'
;
; written by wqd 12/30/2001
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - sou_comb,flist,outfile,fdir=fdir,ftail=ftail'
print,',fsdir=fsdir,fstail=fstail'
return
endif 
if n_elements(fdir) eq 0 then fdir='../xdata/'
if n_elements(ftail) eq 0 then ftail='.fits'
if n_elements(fsdir) eq 0 then fsdir=''
if n_elements(fstail) eq 0 then fstail=''

;setup the source detection parameters:
if n_elements(instr) eq 0 then instr=!instr
;-----
;sources for subtraction:
if n_elements(bsoufile) ne 0 then begin
	sou_fits_info,bsoufile,slist,flow=bflow
	sou_det_params,instr,dim,block,ccd,emin,emax,bmin,bmax $
		,dsfrac,asfrac,psffiled,bpsffile ;,ftail,aimoff,subfac
	if n_elements(eband) ne 0 then emin=bmin(eband-1)
	if n_elements(eband) ne 0 then emax=bmax(eband-1)
	if n_elements(psffile) eq 0 then begin
		if n_elements(eband) ne 0 then psffile=bpsffile(eband-1) $
		else psffile=psffiled
	endif
	if n_elements(perclimit) eq 0 then perclimit=0.85
endif
;-------------

;get the total combined number of sources:
fname=''
snew=1
openr,un,flist,/get
while not eof(un) do begin
	readf,un,fname
        filename=fsdir+fname+fstail
        if exist(filename) eq 0 then stop,filename,' does not exist!!!'
	struct_s=mrdfits(filename,1,shdr)
	;-----------
	fname=fdir+fname+ftail
	print,fname
	file_params,fname,fhdr,fra,fdec,hdr=hdr
		;,expt,nra,ndec,xoff,yoff,roll,aimoff=[0,0]
	if n_elements(bsoufile) ne 0 then $
	 	sou_psf_remove,fra,fdec,struct_s,slist,rlo=rlo,rhi=rhi $
		,perclimit=perc,psffile=psffile,cntrth=cntrlimit,factor=factor
	;-------------
	nrow=n_elements(struct_s)
	if !debug eq 2 then stop
	if nrow ne 0 then begin
		if snew then begin
		 struct_col_add,struct_s, $
                   replicate(strcompress(sxpar(hdr,'OBS_ID'),/rem),nrow) $
			,'OBS_ID','',struct_temp=struct_temp
		 struct_snew=struct_s
		 snew=0
		endif else begin
		 struct_col_add,struct_s, $
                   replicate(strcompress(sxpar(hdr,'OBS_ID'),/rem),nrow) $
			,struct_temp=struct_temp
		 struct_snew=[struct_snew,struct_s]
		endelse
	endif
endwhile
free_lun,un
print,'# of sourcs are combined: ',n_elements(struct_snew)
; output the combined source structure:
comm='merged source list from files listed in '+flist
if n_elements(outfile) ne 0 then $
 sou_struct_fits,struct_snew,outfile,comm=comm,equi=2000
if !debug eq 2 then stop
return
end

