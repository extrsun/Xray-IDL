pro source_list,ih,im,is,jd,jm,js,lists,listb,np_s,np_b,tminv,tmaxv $
,emin=emin,emax=emax,image_c=image_c,equi=equi,dir=dir,seqno=seqno $
,filelist=filelist,exptail=exptail,block=block,rs=rs,rb1=rb1,rb2=rb2 $
,soufile=soufile,factor=factor,slow=slow,flow=flow $
,nosort=nosort,listf=listf,listinfo=listinof,radius=radius,filter=filter
;-
; Obtain a composite list of counts of a source using multiple observations
;*INPUTS:
; ih,im,is,jd,jm,js - RA and Dec of the source position
; equi - equinox (def = 2000)
; dir - directory name containing the image sequence file and source file
; seqno - a vection contains sequence numbers of the files to be included
;		(i.e., [rp123333,rp*]). If not used, filelist need to be 
;		supplied
; filelist - if the keyword seqno is not used, filelist need to be 
;		supplied, containing the sequence numbers with the first line
;		to be the number of files
; exptail - if supplied, the time interval file rp*+'_exp'+exptail+'.dat'
;		will be used in constructing the lists.
;		If not supplied, rp*_actime.dat will be used
; block - defaut block=!block
; rs,rb1,rb2 - the source core, background inner and outer
;			radii, def = 3, 3, 8 arcmin, respectively
; soufile - the name of the file containing source positions,
;		the file should located in the dir
; factor - the factor of the 90% source radius to be used in source 
;	subtractions, def = 2.
; emin, emax - the lower and upper limits of pi channels (def = 20 and 201)
;
;*OUTPUTS:
; lists,listb - structures containing source and background count lists
; tminv,tmaxv - vectors containing lower and upper limits of time intervals
; np_s, np_b - vectors (with the same dimension as tminv)
;	containing source and background areas (changing with time if off-axis
;		angle changes) in units of 0.5" pixels
; listf - count list of the last file
; listinfo - information about the listf (e.g. time intervels)
; 
;*example:
; source_list,3,18,20.7,-66,35,37.,dir='../source/',filelist='n1313.dat'
; ,souf='rp600045_sou.dat',ls,lb,np_s,np_b,tminv,tmaxv
;
; Use source_thist to get the histogram of the source count rates as 
; a function of time.
;
; writen by wqd, Dec 2, 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  source_list,ih,im,is,jd,jm,js,lists,listb'
print,',np_s,np_b,tminv,tmaxv,emin=emin,emax=emax,image_c=image_c,equi=equi'
print,',dir=dir,seqno=seqno,filelist=filelist,exptail=exptail,block=block'
print,',rs=rs,rb1=rb1,rb2=rb2,soufile=soufile,factor=factor'
print,',slow=slow,flow=flow,nosort=nosort,listf=listf,listinfo=listinfo,radius=radius,filter=filter'
return
endif
If n_elements(emin) eq 0 then emin=20
If n_elements(emax) eq 0 then emax=201
if n_elements(equi) eq 0 then equi=2000
if n_elements(block) eq 0 then block=!block
if n_elements(dir) eq 0 then dir=!data_dir ;'/data1/wqd/archives/'
if n_elements(rs) eq 0 then rs=1.
if n_elements(rb1) eq 0 then rb1=1.5
if n_elements(rb2) eq 0 then rb2=4.
;if n_elements(factor) eq 0 then factor=2.
hdim=rb2*120 ;*(block/30.)
trans_radian,ih,im,is,jd,jm,js,sra,sdec,/deg
if equi ne 2000 then precess,sra,sdec,equi,2000

; get the sequence list
if n_elements(filelist) ne 0 then begin
  	openr,un,dir+filelist,/get
	readf,un,nfile
	seqno=strarr(nfile)
	seqnos=''
	for k=0,nfile-1 do begin
	 	readf,un,seqnos
		seqno(k)=strtrim(seqnos,2)
	endfor
	free_lun,un
endif else begin
	if n_elements(seqno) eq 0 then seqno=!seq_no
	nfile=n_elements(seqno)
endelse

	if !instr eq 'p' then begin
		if n_elements(xcenter) eq 0 then xcenter=7679.5
		if n_elements(ycenter) eq 0 then ycenter=7679.5
	endif else begin ;RHRI
		if n_elements(xcenter) eq 0 then xcenter=4095.5
		if n_elements(ycenter) eq 0 then ycenter=4095.5
		emin=0
		emax=0 
	endelse

dim=(2*hdim)/10 ; 5" bin size
image_c=intarr(dim,dim,nfile) ;to store X-ray image in individual observations

; loop over the sequence numbers to get the composite lists
for k=0,nfile-1 do begin
	id=strmid(seqno(k),0,2)
	if !instr eq 'h' then begin
		if strupcase(id) eq 'WH' then $
			env,strmid(seqno(k),2,7),/hri,/mpe else $
			env,strmid(seqno(k),2,7),/hri
	endif else begin
		if strupcase(id) eq 'WP' then	$
			env,strmid(seqno(k),2,7),/mpe else $
			env,strmid(seqno(k),2,7)
	endelse
	image_center,cra,cdec,/deg

	trans_dist,cra,cdec,sra,sdec,xp,yp,/deg
	xp=xcenter+xp & yp=ycenter+yp
	; use the standard coordinate !
       if n_elements(listf) le 1 or n_elements(listinfo) eq 0 then begin 
		;otherwise the listf is provided
	inputs='obseq='+strtrim(!seq_no,2)+',dir='+!data_dir
	if n_elements(emin) ne 0 then inputs=inputs+',emin='+strtrim(emin)
	if n_elements(emax) ne 0 then inputs=inputs+',emax='+strtrim(emax)
	print,'get the count list from ',inputs               
	if n_elements(exptail) eq 0 then $
		tfile=!seq_no+'_actime.dat' else $
		tfile=!seq_no+'_gti'+exptail+'.dat'
        make_list,inputs,listf,listinfo,tfile=tfile
       endif

	; get the source filter for this image
       if n_elements(filter) eq 0 then begin
	if keyword_set(soufile) ne 0 then begin
	   get_filter,flt(512,512)+1.,filter,blow=blow,bhigh=bhigh $
		,block=block,radius=radius $
		,factor=factor,slow=slow,flow=flow,soufile=dir+soufile 
;	   get_filter,filter,blow=blow,bhigh=bhigh,block=block,dim=512 $
;		,factor=factor,slow=slow,flow=flow,soufile=dir+soufile 
	endif else begin
	   print,'No source is subtracted because no source file is provided'
	endelse	
       endif else filter=fltarr(512,512)
	   
       filter_sp,xp,yp,filter,filter_s,block=block,rs=rs,rb1=rb1,rb2=rb2
spec_data_dif,listf,filter_s,0,0,listfs,listfb,numpix_s,numpix_b,block=block 
if !debug eq 1 then stop

	;transfter the coodinate to the image around the source
	listfs.x=listfs.x-(xp-hdim)
	listfs.y=listfs.y-(yp-hdim)
	listfb.x=listfb.x-(xp-hdim)
	listfb.y=listfb.y-(yp-hdim)
	; combine the lists from the image into the composite lists
	if k eq 0 then begin
		lists=listfs  & listb=listfb
		tbeg=listinfo.tbeg
		tend=listinfo.tend  
		sel=where(tbeg ne 0.)
		tminv=tbeg(sel) & tmaxv=tend(sel)
		np_s=tbeg*0.+float(numpix_s) & np_b=tbeg*0.+float(numpix_b)  
		;tbeg*0. is to create a vector  
	endif else begin
		lists=[lists,listfs] & listb=[listb,listfb]
		tbeg=listinfo.tbeg
		tend=listinfo.tend  
		sel=where(tbeg ne 0.)
		tbeg=tbeg(sel) &	tend=tend(sel)
		np_s=[np_s,tbeg*0.+float(numpix_s)] & np_b=[np_b,tbeg*0+numpix_b]  
		tminv=[tminv,tbeg] &	tmaxv=[tmaxv,tend]
	endelse
	list_image,[listfs,listfb],0,0,im,dim,block=10
	image_c(*,*,k)=im
if !debug eq -1 then stop
endfor

if n_elements(nosort) ne 0 then begin
st=sort(tminv)
np_s=np_s(st)
np_b=np_b(st)
tminv=tminv(st)
tmaxv=tmaxv(st)
lists=lists(sort(lists.time))
listb=listb(sort(listb.time))
endif
stop
end
