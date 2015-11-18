pro stitch,filelist,mhdr,mmap,roll=roll,fdir=fdir,ftail=ftail,mtype=mtype,mtoi=mtoi,nointerp=nointerp
;+
; cast  WFPC2 images into a stack of a map with a common fits header
; accounting for the rotation of the images.
;
;*INPUTS:
; filelist - file list: each row contains a file name (or filehead so that
;		file name = filehead+ftail		
; mhdr - the header of the merged map, which can be produced with get_fitshead
;
;*OUTPUTS:
; mmap - merged stack of images
;
;*OPTIONAL Inputs:
; roll - the roll angle (deg from the norminal) of the merged map (def=0.)
; fdir - file directory (def ='../xdata/')
; ftail - characters appended to the filenames in filelist (e.g., '_i')
; mtype - if =1, the merged coordinates are assumed to be Galactic
; offset - if the reference pixel of inhead is not in FORTRAN formation,
; mtoi - if set, use cast_r (mutiple pixels merged into one output bin)
;	otherwise use cast
;
; written by wqd 5/7/2004
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - stitch,filelist,mhdr,mmap,roll=roll,fdir=fdir,ftail=ftail,mtype=mtype,mtoi=mtoi,nointerp=nointerp'
return
endif
if n_elements(roll) eq 0 then roll=0.

if n_elements(mtype) eq 0 then mtype=0
if n_elements(fdir) eq 0 then fdir='../xdata/'
if n_elements(ftail) eq 0 then ftail=''
;-----
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
nfname=fhead+replicate('.fits',n_elements(fname))
append=0
get_headinfo,mhdr,xsize,ysize
nf=n_elements(nfname)
mmap=fltarr(xsize,ysize,nf)
if n_elements(roll) eq 0 then roll=0.
for kk=0,nf-1 do begin
 if exist(nfname(kk)) then begin
     print,nfname(kk)
    inarr=readfits(nfname(kk),inhead)
	get_headcdinfo,inhead,xsize,ysize,scra,scdec,cpx,cpy,del,ang,equi=equi
	get_fitshead,inarr,inheadm,inhead,del=del,cpx=cpx,cpy=cpy
	if keyword_set(mtoi) then begin
		cast_r,mhdr,mmap,inhead,inarr,mtype=mtype $
	 ,offset=offset,nozero=nozero,append=append,avgkey=avgkey 
            endif else begin	
			if mtype eq 1 then gc=3
			cast,'',mhdr,outa=outa,inh=inheadm,ina=inarr,gc=gc $
                          ,nointerp=nointerp,inroll=-ang,roll=-roll
;			if append eq 1 then mmap=mmap+outa else mmap=outa
			mmap(*,*,kk)=outa
	endelse
	append=1
    endif else print,'file ',nfname(kk),' does not exist!!!'
endfor
return
end
