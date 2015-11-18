pro wrtarf,inst,tel,e1,e2,effarea,expo,specresp,arfname=arfname
if n_params(0) eq 0 then begin
 print,'wrtarf,inst,tel,e1,e2,effarea,expo,specresp,arfname=arfname'
 print,'Write a FITS ARF file for ASCA '
 retall
end
if n_elements(arfname) eq 0 then begin
 arfname=' '
 read,'Enter Name of Output ARF file ',arfname
endif
ncol=5l
nrow=(size(specresp))(1)+0l
;create primary header
fxhmake,hdr,/extend,/date
fxwrite,arfname,hdr
;create the extension header
fxbhmake,hdr,nrow,'SPECRESP','name of this binary table extension'
;now create the columns
fxbaddcol,col1,hdr,e1(0),'ENERG_LO',tunit='keV     '
fxbaddcol,col2,hdr,e2(0),'ENERG_HI',tunit='keV     '
fxbaddcol,col3,hdr,effarea(0),'EFFAREA ',tunit='cm2     ' 
fxbaddcol,col4,hdr,expo(0),'EXPOSURE'
fxbaddcol,col5,hdr,specresp(0),'SPECRESP',tunit='cm2     ' 
nh = WHERE(STRMID(HDR,0,8) EQ 'END     ', nend)
hdr=hdr(0:nh(0)-1)
print,'hdr: ',(size(hdr))(1)
s=strarr(12)
s(0)='TELESCOP= ''ASCA    ''           / mission/satellite name'
s(1)='INSTRUME= ''SIS0    ''           / instrument/detector name'
s(2)='DET_ID  =                    0 / detector ID (0-3)'
s(3)='REGCENTX=    0.000000000000000 / data region center (mm)'
s(4)='REGCENTY=    0.000000000000000 / data region center (mm)'
s(5)='REGRIN  =    0.000000000000000 / data region inner radius (mm)'
s(6)='REGROUT =    0.000000000000000 / data region outer radius (mm)'
s(7)='POSCENTX=              0.00000 / x position of source on SIS (mm)'
s(8)='POSCENTY=              0.00000 / y position of source on SIS (mm)'
s(9)='XRT_OFF =              0.00000 / XRT off-axis angle (arcmin)'
s(10)='XRT_AZM =            000.00000 / XRT off-axis angle (arcmin)'
s(11)='END                                                         '
dest=s(1)
strput,dest,inst,11
s(1)=dest
detid=strmid(inst,3,1)
dest=s(2) & strput,dest,detid,29 & s(2)=dest
hdr=[hdr,s]
print,'hdr: ',(size(hdr))(1)
;write the ext header
fxbcreate,unit,arfname,hdr
;write  the data
for i=1l,nrow do begin
 fxbwrite,unit,e1(i-1),col1,i
 fxbwrite,unit,e2(i-1),col2,i
 fxbwrite,unit,effarea(i-1),col3,i
 fxbwrite,unit,expo(i-1),col4,i
 fxbwrite,unit,specresp(i-1),col5,i
endfor
fxbfinish,unit
end
