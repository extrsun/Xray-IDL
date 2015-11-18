pro mkcxrbarf,inst,rspname=rspname,datname=datname,arfname=arfname
if n_params(0) eq 0 then begin
 print,'mkcxrbarf,inst,rspname=rspname,datname=datname,arfname=arfname'
 print,'MAKE a FITS ARF file from a 3-column ascii file '
 print,'inst = SIS0, SIS1, GIS2 or GIS3'
 retall
end
if n_elements(datname) eq 0 then begin
 datname=' '
 read,'Enter name of input ascii datafile '
endif
if n_elements(rspname) eq 0 then begin
 rspname=' '
 read,'Enter name of RMF file for energy grid '
endif
readcol,datname,en,effa,expo
tab=readfits(rspname,h,ext=1)
headconv,h
elo=tbget(h,tab,'energ_lo')
ehi=tbget(h,tab,'energ_hi')
emean=(elo+ehi)/2.
specresp=effa*expo
linterp,en,effa,emean,neweffa
linterp,en,expo,emean,newexpo
newspecresp=neweffa*newexpo
plot,en,specresp,psym=1
oplot,emean,newspecresp
wrtarf,inst,tel,elo,ehi,neweffa,newexpo,newspecresp,arfname=arfname
return
end
