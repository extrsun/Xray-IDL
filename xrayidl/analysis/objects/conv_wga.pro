pro conv_wga,infile,outfile
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - list,infile,outfile,fch=fch'
return
endif
str=''
text1=''
text2=''
text3=''
ns=68907
rad=fltarr(ns)
decd=rad
text=strarr(ns)

openr,un,infile,/get
openw,unout,outfile,/get
formin='(a20,60x,a50)'
formout='(f9.4,a1,f9.4,a50)'
for k=0,8 do readf,un,str ;remove the header
for k=0L,ns-1 do begin
	readf,un,str,text1,text2,format=formin
;	readf,un
	radec=gettok(str,'|')
	radec=radec+' '+gettok(str,'|')
	stringad,radec,rascale,decscale
	rad(k)=rascale & decd(k)=decscale
	text(k)=text1+text2
if !debug eq 1 then stop
endfor
s=sort(rad)
rad=rad(s)
decd=decd(s)
text=text(s)

for k=0L,ns-1 do begin
	 printf,unout, rad(k),' |',decd(k),text(k),format=formout
endfor
free_lun,un
free_lun,unout
end
