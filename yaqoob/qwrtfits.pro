pro qwrtfits,vals,colname=colname,fname=fname,extname=extname
if n_params(0) eq 0 then begin
 print,'qwrtfits,vals,colname=colname,fname=fname,extname=extname'
 print,'Make a quick fits file consisting a binary table with a '
 print,'single 1-D array column '
 retall
end
im=fltarr(2,2)
writefits,fname,im
if n_elements(extname) eq 0 then extname='DUMMY   '
nvals=(size(vals))(1)
fxbhmake,hdr,nvals,strmid(extname,0,8),' name of this binary table extension'
fxbaddcol,col1,hdr,vals(0),strtrim(colname,2)
fxbcreatel,unit,fname,hdr
for i=1l,nvals do fxbwrite,unit,vals(i-1),col1,i 
fxbfinish,unit
return
end
