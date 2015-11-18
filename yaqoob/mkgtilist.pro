pro mkgtilist,list
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin 
 print,' mkgtilist,list  '
 print,' Creates OR updates a list of GTI filenames in the array LIST '
 retall
end
;find out if list already contains filenames
if n_elements(list) eq 0 then begin
 print,'Creating a new GTI file list '
 read,'Enter number of files to add to list ',nfiles
 if nfiles le 0 then goto, fin
 list=strarr(nfiles)
 for k=0,nfiles-1 do begin
	fname=' '
	read,' Enter name of GTI file ',fname & list(k)=fname
 endfor
 goto,fin
endif
;if list is already setup then save these and create a new array
if n_elements(list) gt 0 then begin
 nold=n_elements(list)
 print,'Number of existing GTI files in list ',nold
 forprint,list(0:nold-1)
 read,'Enter number of files to add to list ',nfiles
 if nfiles le 0 then goto, fin
 oldlist=strarr(nold)
 for i=0,nold-1 do oldlist(i) = list(i)
 newlen=nold+nfiles & list=strarr(newlen) 
 for i=0,nold-1 do list(i)=oldlist(i)
 for i=nold,newlen-1 do begin
  fname=' '
  read,' Enter name of GTI file ',fname & list(i)=fname
 endfor
 print,' New GTI file list: '
 forprint,list(0:newlen-1)
 print,' Total number of files: ',newlen
endif
fin: return
end
