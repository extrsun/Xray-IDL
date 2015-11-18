pro mkatable,p,energy,spectrum,filename=filename,maxnp=maxnp
;
;
;-
s=n_elements(energy)
m=4 ; bytes per bin
if n_params(0) eq 0 then begin
	print,'mkatable,p,energy,spectrum,filename=filename,maxnp=maxnp'
	retall
end
if n_elements(filename) eq 0 then begin
	filename=''
	Read,'Output File (w/o extension)',filename 
endif
if n_elements(maxnp) eq 0 then $
	read,'Largest number of parameter values',maxnp
recl=max([s*M+4*p, maxnp*4+40])
filename=filename+'.mod'
openw,lun,filename,recl,/get_lun,/fortran,/fixed
;
; record 1
;
read,'0 for Vax, 1 for Sun',ans
if fix(ans) eq 0 then mach='VAX ' else mach='Sun '
case fix(ans) of
	0: openw,lun,filename,recl,/get_lun,/fortran,/fixed
	else: openw,lun,filename,recl,/get_lun,/f77
endcase
n=long(recl)
i=123456L
r=1.23456E07 & r=float(r)
m=fix(m)
s=long(s)
p=long(p)
model=''
read,'Model Name',model
len=strlen(model)
if len lt 12 then model=model+padder(12-len) $   
	else model=strmid(model,1,12)
read,'Start energy for model', e0 & e0=float(e0)
read,'End   energy for model', e1 & e1=float(e1)
H=0L
flag='F'
writeu,lun,mach,i,r,n,m,s,p,model,e0,e1,h,flag
;
; record 2 - P+1
;
for i=0,p-1 do begin
	pa=''
	read,'Parameter name',pa
	len=strlen(pa)
	if len lt 12 then pa=pa+padder(12-len) $
       		 else pa=strmid(pa,1,12)
	read,'Initial value', iv & iv=float(iv)
	read,'Minimum value', mv & mv=float(mv)
	read,'Low     value', lv & lv =float(lv)
	read,'High    value', hv & hv =float(hv)
	read,'Max     value', maxv & maxv=float(maxv)
	read,'delta        ', dv & dv=float(dv)
	read,'Number of parameter values',np & np=fix(np)
	read,'Interpolation scheme: 0=lin, 1=log',ip & ip=fix(ip)
	parval=fltarr(np)
	for j=0,np-1 do begin
		read,'Parameter value',pv & parval(j)=float(pv)
	endfor 
	writeu,lun,pa,iv,mv,lv,hv,maxv,dv,np,ip,parval
endfor
;
; record P+2
;
writeu,lun,energy
;
; record P+3 - p+2+np(1)*np(2)*...np(p)
;
writeu,lun,spectrum
free_lun,lun
return
end
