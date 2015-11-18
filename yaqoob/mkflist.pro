pro mkflist,inst,remfb,dir=dir,fname=fname
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin
 print,'mkflist,inst,remfb,dir=dir,fname=fname'
 print,'Make a list of filenames in the directory DIR for INST'
 print,'FNAME:	output filename'
 print,'DIR  :	directory containing files '
 print,'INST :	S0, S1, G2 or G3 '
 print,'REMFB:  If remfb=1, remove converted faint mode files'
 retall
endif 
inst=strmid(inst,0,2)
print,'Making list of files for ',inst
com='ls -1 '+dir+'*'+inst+'*fits* |grep -v HK > temp.ascii' 
spawn,com
fmax=1000
datname=strarr(fmax)
openr,1,'temp.ascii'
i=0
while (not eof(1)) do begin
 name = ' '
 readf,1,name
 if (strmid(dir,0,1)) eq ' ' then datname(i) = dir+name else $
 datname(i) = name
	i=i+1
 print,i,' ',datname(i-1)
endwhile
close,1
nfiles=i
root=strarr(fmax)
openw,1,fname
 for k=0,nfiles-1 do begin
	fpos=strpos(datname(k),'fits')
	root(k)=strmid(datname(k),0,(fpos-4))
      if k eq 0 then printf,1,strmid(datname(k),(fpos-27),31)
       if k gt 0 then begin
	if root(k) eq root(k-1) and remfb eq 1 then begin
	 com2='rm -f '+datname(k)
	 spawn,com2
	endif
  	if root(k) ne root(k-1) then $
	printf,1,strmid(datname(k),(fpos-27),31) 
       endif
 endfor
close,1
com='rm -f temp.ascii' 
spawn,com
return
end
