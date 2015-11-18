pro arfgrid,seqfile,nx=nx,ny=ny,x1=x1,x2=x2,y1=y1,y2=y2,rmfnames=rmfnames
if n_params(0) eq 0 then begin

print,'arfgrid,seqfile,nx=nx,ny=ny,x1=x1,x2=x2,y1=y1,y2=y2,rmfnames=rmfnames'
 print,'This routine makes scripts to run ASCAARF many times for each'
 print,'spectral file over a grid of values of the optical axes for '
 print,'each instrument. It is a driver for the routine MKARFGRID '
 print,'which works on a single spectral file and single sequence'
 print,'ARFGRDDRIVE takes an ascii file containing a list of sequence '
 print,'numbers (seqfile) and then for each sequence number will do as follows:'
 print,'	1. For each instrument s0 s1 g2 g3 (=detid)
 print,'	 search for spectral files of type *seq#*detid*src*.p*'
 print,'	2. For each spectral file, run through a grid of'
 print,'	 optical axis values with Ox ranging from x1 to x2 in '
 print,'	 nx steps and values of Oy ranging from y1 to y2 in ny'
 print,'	 steps. x and y (array(4)) are in DET coords. Defaults are: '
 print,' '
 print,'			x1	x2	y1 	y2
 print,'	SIS0		625	700	521	596
 print,'	SIS1		580	656	696     772
 print,'	GIS2		128	138	126	136
 print,'	GIS3		115 	125	129	139	
 print,'
 print,' 	DEFAULTS for NX: [8,8,10,10]'
 print,'		     NY: [8,8,10,10]'
 print,' All files are searched for in the current directory '
 print,'RMFNAMES(4) is a string array '
 print,'The defualt RMF filenames are:'
 print,'/FTP/caldb/data/asca/sis/cpf/94nov9/s0c1g0234p40e1_512v0_8i.rmf'
 print,'/FTP/caldb/data/asca/sis/cpf/94nov9/s1c3g0234p40e1_512v0_8i.rmf'
 print,'/FTP/caldb/data/asca/gis/cpf/95mar06/gis2v4_0.rmf'
 print,'/FTP/caldb/data/asca/gis/cpf/95mar06/gis3v4_0.rmf'
 print,' '
 print,'ARFNAMES(nx,ny,nseq,4) is an output grid of arf filenames '
 retall
end
if n_elements(x1) eq 0 then x1=[625.,580.,128.,115.]
if n_elements(x2) eq 0 then x2=[700.,656.,138,125.]
if n_elements(y1) eq 0 then y1=[521.,696.,126.,129.]
if n_elements(y2) eq 0 then y2=[596.,772.,136.,139.]
if n_elements(nx) eq 0 then nx=[8,8,10,10]
if n_elements(ny) eq 0 then ny=[8,8,10,10]
instid=['s0','s1','g2','g3']
if n_elements(rmfnames) eq 0 then begin
 rmfnames=strarr(4)
 rmfnames(0)=$
'/FTP/caldb/data/asca/sis/cpf/94nov9/s0c1g0234p40e1_512v0_8i.rmf'
 rmfnames(1)=$
'/FTP/caldb/data/asca/sis/cpf/94nov9/s1c3g0234p40e1_512v0_8i.rmf'
rmfnames(2)=$
'/FTP/caldb/data/asca/gis/cpf/95mar06/gis2v4_0.rmf'
rmfnames(3)=$
'/FTP/caldb/data/asca/gis/cpf/95mar06/gis3v4_0.rmf'
endif
;step through each sequence
openr,1,seqfile
iseq=0l
dummy=' '
seqname=strarr(10000l)
while not eof(1) do begin
 readf,1,dummy
 seqname(iseq)=dummy 
 iseq=iseq+1l
endwhile
close,1
dot=strpos(seqfile,'.')
rootname=strmid(seqfile,0,dot)
openw,3,rootname+'_master.arfscript'
openw,5,rootname+'_xspec.script'
printf,5,'log '+rootname+'_xspec.log'
;for each instrument and sequence find the spectral file
 for j=0l,3 do begin
  print,'Doing ',instid(j)
  for i=0l,iseq-1l do begin
   seqname(i)=strtrim(seqname(i),2)
   print,'Doing sequence # ',i,seqname
  specfiles=findfile('*'+seqname(i)+'*'+instid(j)+'*src*.p*',count=nspec)
 if nspec gt 1 then begin
  print,'More than 1 spectral file found: taking 1st one'
 endif
  specname=specfiles(0)
  rmfname=rmfnames(j)
;construct the name of the script file for this part
 arfscript=seqname(i)+'_'+instid(j)+'.arfscript'
 arfnamfile=seqname(i)+'_'+instid(j)+'.arfnames'
 printf,3,'source '+strtrim(arfscript,2)
mkarfgrid,specname,rmfname,arfs,x1(j),x2(j),nx(j),y1(j),y2(j),ny(j),xl,xh,yl,yh,arfscript=arfscript
 openw,4,arfnamfile
 for k=0l,nx(j)-1l do begin
  for kk=0l,ny(j)-1l do begin
   printf,4,arfs(k,kk)
  endfor
 endfor
 close,4
;now write the xspec script
 printf,5,'data '+specfiles(1)
 printf,5,'ig bad '
 printf,5,'ig 0.0-0.5'
 printf,5,'ig 10.-100.'
 for k=0l,nx(j)-1l do begin
  for kk=0l,ny(j)-1l do begin
    printf,5,'arf ',arfs(k,kk)
    printf,5,'@model.xcm'
    printf,5,'fit 300 0.0005'
    printf,5,'flux 0.5 10.0'
    printf,5,'flux 0.5 5.0'
    printf,5,'flux 0.5 2.0'
    printf,5,'flux 2.0 10.0'
  endfor
 endfor
 endfor
endfor
close,5
close,3
return
end
