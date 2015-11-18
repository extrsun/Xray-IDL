pro rosat_info,souno,ra,dec,inst,expo,name $
,tlow=tlow,gllo=gllo,glhi=glhi,text=text,infile=infile,outfile=outfile
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - rosat_info,souno,ra,dec,inst'
print,',expo,name,tlow=tlow,gllo=gllo,glhi=glhi,text=text,infile=infile,outfile=outfile'
retall
endif
;
if n_elements(tlow) eq 0 then tlow=1.e4
if n_elements(gllo) eq 0 then gllo=0.
if n_elements(instch) eq 0 then instch='PSPC'
if n_elements(infile) eq 0 then $
		fname='/data4/data/masterlog.txt' else fname=infile
openr,un,fname,/get_lun

n_source=4000
text=strarr(n_source)
souno=strarr(n_source)
expo=lonarr(n_source)
name=strarr(n_source)
last=strarr(n_source)
ra=dblarr(n_source)
dec=dblarr(n_source)
k=0
str=''
aa=''
   readf,un, str
while not EOF(un) do begin
   readf,un, str
   text(k)=str
   aa=gettok(str,'|')
   inst=strtrim(gettok(str,'|'),2)
   souno(k)=gettok(str,'|')
   aa=gettok(str,'|')
   aa=gettok(str,'|')
   name(k)=gettok(str,'|')
   last(k)=gettok(str,'|')
   aa=gettok(str,'|')
   expo(k)=gettok(str,'|')
   ra(k)=gettok(str,'|')
   dec(k)=gettok(str,'|')
   glactc,ra(k)*(12./180.),dec(k),2000,gl,gb,1
   if abs(gb) gt gllo and abs(gb) lt glhi and expo(k) ge tlow and inst eq instch  $
	then begin
   k=k+1
   endif
endwhile
free_lun,un
n_source=k
if n_source eq 0 then begin
		print,'the file contains no source, return'
		return
endif
text=text(0:n_source-1)
souno=souno(0:n_source-1)
ra=ra(0:n_source-1)
dec=dec(0:n_source-1)
expo=expo(0:n_source-1)
name=name(0:n_source-1)
last=last(0:n_source-1)

if n_elements(outfile) ne 0 then begin
	openw,un,outfile,/get_lun
	for k=0,n_source-1 do begin
		printf,un,ra(k),dec(k),expo(k),souno(k),name(k),last(k) $
		,format='(2f11.5,I7,a13,2a15)'
	endfor 
	free_lun,un
endif
return
end