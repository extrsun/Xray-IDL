pro list_merge,filelist,mhdr,mlist,frav,fdecv,fexptv,emin=emin,emax=emax $
,ccd=ccd,fdir=fdir,mtype=mtype,ftail=ftail $
,slow=slow,soufile=soufile,instr=instr,subfac=subfac,psffile=psffile $
,eband=eband,row=row,tagin=tagin,filtero=filtero,mgti=mgti
;+
; merge events in a list of observations into a common event structure
;
;*INPUTS:
; filelist - file list: each row contains an event file name
; mhdr - the header of the merge list
;
;*OUTPUTS:
; mlist - merged event list (structure)
; frav, fdev, fexptv - vectors containing RA, Dec, and expt of individual
;				observations
;*OPTIONAL Inputs:
; emin, emax - lower and upper limits of event energy (eV)
; ccd - selected ccd ID numbers
; fdir - file directory (def ='../xdata/')
; mtype - if =1, the merged coordinates are assumed to be Galactic
; ftail - characters appended to the filenames in filelist (e.g., '.fits')
; slow - the source signal-to-noise threshold for source subtraction
; soufile - source file name
; instr - instrument name (def = !instr)
; subfac - subtraction factor of the source energy-encircled radius as
;		defined in sou_det_params
; psffile = psf file name. Def = psffile from sou_det_params if eband is not
;		specified, otherwise use the bpsffile(eband-1)
; eband - energy band (1,2,3). If given bsffile and emin(eband-1) and
;		emax(eband-1) are used
; row - definition of the structure 
;	(def: row = {time:0.0D0,x:0.,y:0.,energy:0,ccd_id:0})
; tagin - needed if the tag names in the event file are different 
;	from those defined in row.
; filtero - if given, only counts in regions with filter > 0 will be
;           selected, useful to cut out the outer region for example.
; mgti - the merged GTI structure
;
; written by wqd 2/22/2001
; add the keyword for the gti output, wqd, 7/23/07
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - list_merge,filelist,mhdr,mlist,frav,fdecv,fexptv'
print,',emin=emin,emax=emax,ccd=ccd,fdir=fdir,mtype=mtype,ftail=ftail'
print,',slow=slow,soufile=soufile,instr=instr,subfac=subfac,psffile=psffile'
print,',eband=eband,row=row,tagin=tagin,filtero=filtero,mgti=mgti'
return
endif
if n_elements(fdir) eq 0 then fdir='../xdata/'
if n_elements(ftail) eq 0 then ftail=''
mrefp=sxpar(mhdr,'crpix*')
mcrval=sxpar(mhdr,'crval*')
mnaxis=sxpar(mhdr,'naxis*')
if n_elements(mtype) eq 0 then begin 
	;see if the conversion is to the Galactic coordi
	ctype=sxpar(mhdr,'ctype*') 
	if strupcase(ctype(0)) eq 'GLOG---TAN' then mtype=1 else mtype=0
endif
;-----
;sources for subtraction:
nfilter=n_elements(filtero)
nsource=n_elements(soufile)
if nsource ne 0 or nfilter ne 0 then begin
    if nsource ne 0 then $
      source_info,sn,sra,sdec,ston,cntr,slow=slow,souf=soufile,/deg,/deci
    ;setup the source detection parameters:
    if n_elements(instr) eq 0 then instr=!instr
    sou_det_params,instr,dim,block,ccd,bmin,bmax $
    	,dsfrac,asfrac,psffiled,bpsffile ;,ftail,aimoff,subfac
    if n_elements(eband) ne 0 then emin=bmin(eband-1)
    if n_elements(eband) ne 0 then emax=bmax(eband-1)
    if n_elements(psffile) eq 0 then begin
    	if n_elements(eband) ne 0 then psffile=bpsffile(eband-1) $
    	else psffile=psffiled
    endif
    if nfilter ne 0 then filter=filtero
endif
;-------------
if n_elements(row) eq 0 then $ 
	row = {x:0.,y:0.,energy:0,time:0.0D0,ccd_id:0,mx:0.,my:0.,fid:0}
n_tag=n_tags(row)-3 ;last three tages are added
if n_elements(tagin) eq 0 then begin
	tagin=tag_names(row)
	tagin=tagin(0:n_tag-1) 
endif
mlist=row
row_gti={start:0.0d,stop:0.0d}
mgti=row_gti

fexptv=0
frav=0.d
fdecv=0.d
openr,un,filelist,/get
fname=''
;k=0
while not eof(un) do begin
	readf,un,fname
	dfname=fdir+fname+ftail
	print,dfname
	file_params,dfname,fhdr,fra,fdec,expt,nra,ndec,xoff,yoff,roll $
		,hdr=hdr,aimoff=[0,0]
	;get the count list
;	k=k+1
        obsid=sxpar(hdr,'OBS_ID')
        ;aviod string OBSID (as 'Merged' or 'total')
        sz=size(obsid)
        if sz[sz[0]+1] ne 2 then obsid=0 else $
        if max(obsid) eq 0 then obsid=gettok(fname,'_')
           ;no obs_id key in the background file
	sz=size(obsid) & if sz[sz[0]+1] ne 2 then obsid=0
	row.(n_tag+2)=obsid
	list_xray,dfname,flist,emin=emin,emax=emax,ccd=ccd,row=row $
		,tagin=tagin,n_tag=n_tag
	if nsource ne 0 or nfilter ne 0 then begin
            if nsource ne 0 then begin
              filter=source_sub_v(filtero,fra,fdec,sra,sdec,cntr $
	       ,block=block,fac=subfac,/deg,perc=asfrac,psffile=psffile) 
          endif 
	    xmin=!pref+xoff-dim*block/2 ;low left pixel of the subimage
	    ymin=!pref+yoff-dim*block/2
	    list_image,flist,xmin,ymin,cb,dim,block=block $
	       ,emin=emin,emax=emax,filter=filter,sel=sel
	    flist=flist(sel)
if !debug eq 2 then stop
	endif

	list_cast,mcrval(0),mcrval(1),fhdr,flist,flistnew,mxv,myv,/xyreal $
	 ,/xykeep,pixsize=pixsize,mrefp=mrefp,mtype=mtype,naxis=naxis,msel=msel
	if msel ne 0 then begin
		flistnew.(n_tag)=mxv
		flistnew.(n_tag+1)=myv
		mlist=[mlist,flistnew]
		frav=[frav,fra]
		fdecv=[fdecv,fdec]
		fexptv=[fexptv,expt]
            endif else print,'No count in this field for this OB!!!'
                                ;read the gti, assuming to be the same
                                ;for all CCDs (OK for ACIS data)
            list_xray,dfname,gti,row=row_gti,ext=2
            mgti=[mgti,gti]
if !debug eq 2 then stop
endwhile
free_lun,un
if n_elements(mlist) eq 0 then stop,'no count is found!!!'
mlist=mlist(1:*)
mgti=mgti(1:*)
frav=frav(1:*)
fdecv=fdecv(1:*)
fexptv=fexptv(1:*)
return
end
