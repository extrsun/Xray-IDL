pro updategiseve,newpi,evename=evename
if n_params(0) eq 0 then begin
 print,'updategiseve,newpi,evename=evename'
 print,'Repalce the PI values in a GIS events file with new values'
 print,'contained in the IDL array NEWPI'
 retall
endif
;create temporary fits files with the old and new pi values
dot=strpos(evename,'.')
tname1=strmid(evename,0,dot)+'.oldtmp'
tname2=strmid(evename,0,dot)+'.newtmp'
tab=readfits(evename,h1,ext=1)
oldpi=tbget(h1,tab,'PI')
colname='PI'  
qwrtfits,oldpi,colname=colname,fname=tname1,extname='OLDPI'
colname='NEWPI'
qwrtfits,newpi,colname=colname,fname=tname2,extname='NEWPI'
fstr='faddcol '+strtrim(evename,2)+' '+strtrim(tname2,2)+' NEWPI'
print,fstr
spawn,fstr,/sh 
;if the newpi column already exists, make sure it carries the
;latest values
fstr='fcalc '+strtrim(evename,2)+'[1] '+strtrim(evename,2)+$
' PI NEWPI clobber=yes' 
print,fstr
spawn,fstr,/sh
return
end
