pro xtefilt,outname,obsidlist=obsidlist,fpath=fpath,apidnm=apidnm,outdir=outdir
if n_params(0) eq 0 then begin
 print,'xtefilt,outname,obsidlist=obsidlist,fpath=fpath,apidnm=apidnm,outdir=outdir'
 print,'Take an input ascii file with the obsid strings'
 print,'and the full path to the FITS files and output a script which runs '
 print,'xtefilt on all of the obsid '
 print,'obsidlist = name of file containing the values of obsid'
 print,'fpath = name of path for FITS files '
 print,'apidnm = name of file containing apids '
 print,'outdir = directory to place output script '
 retall
end
if n_elements(fpath) eq 0 then begin
 fpath=' '
 read,'Enter full pathname of FITS directory incl last slash ',fpath
endif
if n_elements(obsidlist) eq 0 then begin
 obsidlist=' '
 read,'Enter  name of modelfile incl path',modelfile
endif
if n_elements(outdir) eq 0 then begin
 outdir=' '
 read,'Enter name of directory to place script ',outdir
endif
if n_elements(apidnm) eq 0 then begin
 apidnm=' '
 read,'Enter name of directory to place created bgd files ',apidnm
endif
openr,1,obsidlist
ifile=0l
dum=' '
obsid=strarr(1000)
while (not eof(1)) do begin
 readf,1,dum
 obsid(ifile)=dum
 print,obsid(ifile)
 ifile=ifile+1l
endwhile
print,'Number of OBSID: ',ifile
close,1
openw,2,outdir+outname
for k=0l,ifile-1l do begin
printf,2,'xtefilt -o '+obsid(k)+' -p '+fpath+ $
 ' -a '+apidnm $
 +' -t 16.0 '
 printf,2,' '
; printf,2,'infile='+filename(k),' /'
; printf,2,'outfile='+backfile, ' /'
; printf,2,'modelfile='+modelfile,' /'
endfor
close,2
return
end
