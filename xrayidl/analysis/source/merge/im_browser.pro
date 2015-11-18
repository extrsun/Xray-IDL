pro im_browser,filelist,fdir=fdir,ftail=ftail,slow=slow,souf=soufile,instr=instr,subfac=subfac,psffile=psffile,slo=slo,shi=shi
;+
; browse a list of images
;
;*INPUTS:
; filelist - file list: each row contains a file name (or filehead so that
;		file name = filehead+ftail		
;
;*OUTPUTS:
;	TV images
;
;*OPTIONAL Inputs:
; fdir - file directory (def ='../xdata/')
; ftail - characters appended to the filenames in filelist (e.g., '.fit')
; soufile - source file name
; slow - S/N ratio threshold for source subtraction
; instr - instrument type (def=!instr)
; subfac - a factor of the default source subtraction radius (90%)
; psffile - psf file name (def = source detection default)
; slo, shi - lower and upper limits of the bscale for tv the image
;*Example:
; im_browser,'flist.dat',ftail='_i2.fits',fdir='' $
; ,sou='acisf00945N002_evt2_map70_mlm_ratio',psf='psf_po0.7_acisi_0.dat'
;
; written by wqd 2/29/2001
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - im_browser,filelist,fdir=fdir,ftail=ftail,slow=slow'
print,',souf=soufile,instr=instr,subfac=subfac,psffile=psffile'
return
endif
if n_elements(instr) eq 0 then instr=!instr
if n_elements(fdir) eq 0 then fdir='../xdata/'
if n_elements(ftail) eq 0 then ftail=''
;-----
;sources for subtraction:
if n_elements(soufile) ne 0 then begin
	;source_info,sn,sra,sdec,slow=slow,souf=soufile,/deg
	row={ra:0.0d,dec:0.0d,cntrb:0.}
	list_xray,soufile,slist,row=row
	;setup the source detection parameters:
	sou_det_params,instr,dim,block,ccd,emin,emax,bmin,bmax $
		,dsfrac,asfrac,psffiled,bpsffile,ftaild,aimoff,subfacd
	if n_elements(psffile) eq 0 then psffile=psffiled
	if n_elements(subfac) eq 0 then subfac=subfacd
endif
;-------------
openr,un,filelist,/get
fname=''
k=0
fnamev=strarr(1000)
while not eof(un) do begin
	readf,un,fname
	fnamev(k)=fname
	k=k+1
endwhile
nf=k
fnamev=fdir+fnamev+ftail

for k=0,nf-1 do begin
	print,'k, fnamev(k) = ',k,' ', fnamev(k)
	inarr=readfits(fnamev(k),inhead)
	cra=sxpar(inhead,'crval1')
	cdec=sxpar(inhead,'crval2')
	if n_elements(soufile) ne 0 then $
	 inarr=source_sub_v(inarr,cra,cdec,slist.ra,slist.dec,slist.cntrb,block=block $
	 ,fac=subfac,/deg,perc=asfrac,psffile=psffile) ;,cra=cra,cdec=cdec)
		;assuming the image center is the same as the axis
		;and the default block 
	tv,bscale(inarr,slo,shi)
	print,'Click the mouse to make change:'
	print,' Left -> next; Middle -> exit; Right -> to choose'
	pause
	case !err of 
		1: inarr_s=inarr
		2: goto, out
		4: begin
			print,'Give a number of the file to show'
			read,k
			k=k-1
			end
	endcase
endfor
out:
return
end
