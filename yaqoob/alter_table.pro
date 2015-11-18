pro alter_table

;this one will read in the table file, alter the header, 
;and write it out again.
;ideally it will have the flag set to include redshift as a parameter.
;number of lines =2+numparams+number of computed params

;this one is different to make it more automatic.

;get the name of the table from the screen

tabname='                       '

print,'the name of the table file'
read,tabname

print,tabname

;choose a temporary record length to get the real record lenth from
;the header.

temprecl=1000

temparr=bytarr(temprecl,1)

;read just the first several lines of the header, to get the record length.

openr,lun,tabname,temprecl,/get_lun
readu,lun,temparr
free_lun,lun

;the record length is located at the 13th place, and the number of 
;parameters is located in the 22nd place

temparr1=temparr(0:temprecl-1,0)

recl=long(temparr1,12)
p=long(temparr1,22)
zflag=long(temparr1,50)
headrecs=long(temparr1,46)

print,'the record length and number of parameters are ',recl,p
print,'the zflag and additional header records',zflag,headrecs
;get the number of parameter values for each parameter

temparr=bytarr(recl,p+1)

openr,lun,tabname,recl,/get_lun
readu,lun,temparr
free_lun,lun

numpar=lonarr(p)

for i=0,p-1 do begin
	temparr1=temparr(0:recl-1,i+1)
	numpar(i)=long(temparr1,36)
	parname=string(temparr1(0:11))
	print,parname
	print,numpar(i)
endfor

;so the number of lines in the table should be 2+p+headrecs+numpar*numpar...

ntemp=long(1)
for i=0,p-1 do begin
	ntemp=ntemp*numpar(i)
endfor

numlines=ntemp+2+p+headrecs

print,'the number of lines',numlines

;make the array and dump the table.

thearr=bytarr(recl,numlines)

openr,lun,tabname,recl,/get_lun
readu,lun,thearr
free_lun,lun

;just change one point from 0 to 1 will set the flag.
print,thearr(50,0)
thearr(50,0)=1

;now write it back out to a file with _z at the end

ntemp=strpos(tabname,'.')

tabnamenew=strmid(tabname,0,ntemp)+'_z'+ $
strmid(tabname,ntemp,strlen(tabname))

print,tabnamenew

openw,lun,tabnamenew,recl,/get_lun
writeu,lun,thearr
free_lun,lun

return
end


