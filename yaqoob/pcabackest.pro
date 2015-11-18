pro pcabackest,fname,outname,outdir=outdir,backdir=backdir,modelfile=modelfile,bckext=bckext,xfllist=xfllist,xflmap
if n_params(0) eq 0 then begin
 print,'pcabackest,fname,outname,outdir=outdir,backdir=backdir,modelfile=modelfile,bckext=bckext,xfllist=xfllist,xflmap'
 print,'Take an input ascii file with the names of the XTE histogram'
 print,'files (incl full path) and output a script which runs '
 print,'pcabackest on all of them'
 print,'e.g. modelfile = pca_bkgd_q6_e03v01.mdl'
 print,'e.g. modelfile = pca_bkgd_activ_e03v02.mdl'
 print,'e.g. modelfile = pca_bkgd_xray_e03v01.mdl'
 print,'outdir = directory to place output script '
 print,'backdir = directory to place bgd files '
 print,'bckext = extenstion of output filename - e.g. bckext="bck"'
 print,'xfllist = ascii file with xfl filenames '
 print,'xflmap = integer array with correspondece between data files '
 print,'and xfl files '
 retall
end
;if n_elements(dir) eq 0 then begin
; dir=' '
; read,'Enter full name of directory incl last slash ',dir
;endif
if n_elements(modelfile) eq 0 then begin
 modelfile=' '
 read,'Enter  name of modelfile incl path',modelfile
endif
if n_elements(outdir) eq 0 then begin
 outdir=' '
 read,'Enter name of directory to place script ',outdir
endif
if n_elements(backdir) eq 0 then begin
 backdir=' '
 read,'Enter name of directory to place created bgd files ',backdir
endif
if n_elements(xfllist) eq 0 then begin
 xfllist=' '
 read,'Enter name of ascii file containing names of xfl files ',xfllist
endif
nfilmax=1000l
xflnames=strarr(nfilmax)
ixfl=0l
openr,1,xfllist 
 while (not eof(1)) do begin
 dum=' '
 readf,1,dum
 xflnames(ixfl)=dum
 ixfl=ixfl+1l
endwhile
print,'XFL files read: ',ixfl
close,1
openr,1,fname
ifile=0l
dum=' '
filename=strarr(1000)
while (not eof(1)) do begin
 readf,1,dum
 filename(ifile)=dum
 print,filename(ifile)
 ifile=ifile+1l
endwhile
print,'Number of files: ',ifile
close,1
openw,2,outdir+outname
for k=0l,ifile-1l do begin
 i1=strpos(filename(k),'pca',0)
 l=strlen(filename(k)) & fl=l-i1-4
 backfile=strmid(filename(k),i1+4,fl)+bckext 
printf,2,'pcabackest infile='+filename(k)+' outfile='+backdir+backfile+ $
 ' modelfile='+modelfile+' filterfile='+xflnames(xflmap(k)-1l) $
 +' interval=16.0 propane=no layers=yes gaincorr=no fullspec=no'+$
 ' syserr=no'
 printf,2,' '
; printf,2,'infile='+filename(k),' /'
; printf,2,'outfile='+backfile, ' /'
; printf,2,'modelfile='+modelfile,' /'
endfor
close,2
return
end
