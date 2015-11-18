pro tableget,fname,vname,pname,xten,xte=xte
if n_params(0) eq 0 then begin
 print,'tableget,fname,vname,pname,xten,xte=xte'
 print,'Generalized routine to read fits table '
 print,'If XTE=0 use tbget, IF XTE=1 use fxbread'
 print,'FNAME - name of FITS file '
 print,'VNAME - Name of variable assigned to the array to be read'
 print,'PNAME - Name of parameter in the FITS file '
 print,'XTEN  - number of extension ' 
 retall
end
nanval=-1.0e30
if xte eq 0 then begin
 tab=readfits(fname,h,ext=xten)
 vname=tbget(h,tab,pname)
endif
if xte eq 1 then begin
 fxbopen,unit,fname,xten,h
 fxbread,unit,vname,pname,nanvalue=nanval
 fxbclose,unit
endif
return
end
