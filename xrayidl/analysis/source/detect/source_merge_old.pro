pro source_merge,infile,outfile,slow=slow,flow=flow,text=text,dir=dir,ext=ext $
	,soufile=soufile,noout=noout,sradius=sradius,self=self
;-
; Merge soufiles created in source detections in individual image files
; or in individual bands
;
; infile - input file name containing a list of the image seq numbers and 
; 	will not be used if soufile is given
; dir - directory where infile is located (def dir = '')
; ext - extension of the source files to be attached to the seq numbers
; 	(def ext='_souratio.dat')
; soufile - a vector containing the source file names (including directories
;	relative to the current directory
; slow, flow - S/N and flux selection criteria for the sources to be merged
; sradius - the maximum core radius in which sources are considered to 
;	be duplicate ones (in arcmin). Def sradius=1 
;*outputs:
; outfile - output file name to contain the merged source list
; noout - if set, no output file 
; text - the merged source list
;*example
; source_merge,'rcra_sou_file.dat','rcra_sou,dat',dir='../rcra/',ext='_souratio.dat',sradius=1.
; writen by WQD, Oct 17, 1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_merge,infile,outfile,slow=slow,flow=flow'
print,',text=text,dir=dir,ext=ext,soufile=soufile,noout=noout'
return
endif
if n_elements(sradius) eq 0 then sradius=1.
if n_elements(dir) eq 0 then dir=''
if n_elements(ext) eq 0 then ext='_souratio.dat'
bin_arcmin=!block*0.5/60.

; get a list of the source file names
if n_elements(soufile) eq 0 then begin
 soufile=strarr(100)
 imfile=''
 nf=0
 openr,un,dir+infile,/get_lun
 while eof(un) ne 1 do begin
  readf,un,imfile
  data_dir='/data1/wqd/archives/rp'+imfile+'/'
  soufile(nf)=data_dir+'rp'+imfile+ext
  nf=nf+1
 endwhile
 free_lun,un
endif else nf=n_elements(soufile)

; get a list of all the sources in the files
rat=[-999]
dect=[-999]
xpt=[-999]
ypt=[-999]
textt=['']
for k=0,nf-1 do begin
  source_info,sno,ra,dec,sigma,cntr,xp,yp,soufile=soufile(k) $
	,slow=slow,flow=flow,text=text,ns=ns,self=self
  if ns ne 0 then begin
   rat=[rat,ra]
   dect=[dect,dec]
   xpt=[xpt,xp-255.5]
   ypt=[ypt,yp-255.5]
   textt=[textt,text]
  endif
endfor
rat=rat(1:*)
dect=dect(1:*)
xpt=xpt(1:*)
ypt=ypt(1:*)
textt=textt(1:*)
ns=n_elements(rat)

; get a rough estimate of error circle for each source
dis=sqrt(xpt*xpt+ypt*ypt > 1.e-10)*bin_arcmin
blow=4 & bhigh=7
detect_params,dis,core,blow=blow,bhigh=bhigh

; exclude duplicate sources
for k=ns-1,1,-1 do begin
	trans_dist,rat(k),dect(k),rat(0:k-1),dect(0:k-1),px,py
	sep=(px*px+py*py)/(120*120)
	seperr=(core(0:k-1)*core(0:k-1) < core(k)*core(k))
	if n_elements(sradius) ne 0 then seperr=seperr < sradius*sradius
	sel= where(sep lt seperr and (dis(k) ge dis(0:k-1)),nsel)
	if nsel ne 0 then begin
		print,'k= ',textt(k),sep(sel),seperr(sel)
		print,textt(sel)
		remove,[k],textt,rat
	endif
endfor
; output the new source list
ns=n_elements(textt)
textt=textt(sort(rat))
nums=string(indgen(ns)+1)
textr=strmid(nums,5,3)+strmid(textt,3,130)
if keyword_set(noout) eq 0 then openw,unout,dir+outfile,/get_lun
for k=0,ns-1 do begin
if keyword_set(noout) eq 0 then printf,unout,textr(k)
	print,textr(k)
endfor
text=textr
free_lun,unout
end