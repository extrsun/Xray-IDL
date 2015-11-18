pro putnewoptx,inst,specname
if n_params(0) eq 0 then begin
 print,'putnewoptx,inst,specname'
 print,'Insert new optical axes values into a spectral file'
 print,'Values are : [s0,s1,s2,s3] '
 print,'OPTIC1  [675.0,632.5,132.25,120.25]
 print,'OPTIC2  [595.0,790.0,132.0,133.0]
 retall
end
optic1=[675.0,632.5,132.25,120.25]
optic2=[595.0,790.0,132.0,133.0]
xval=optic1(inst) & yval=optic2(inst) 
fstr='fparkey value = '+strtrim(string(xval),2)+' fitsfile = '+specname+'+0 keyword = OPTIC1'
spawn,fstr,/sh
fstr='fparkey value = '+strtrim(string(yval),2)+' fitsfile = '+specname+'+0 keyword = OPTIC2'
spawn,fstr,/sh
return
end
