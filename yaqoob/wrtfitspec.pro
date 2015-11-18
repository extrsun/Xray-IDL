pro wrtfitspec,h0,h1,h2,cts,gti,wmap,specname=specname,hdr,hdr1,hdr2
;Author T. Yaqoob - Oct 1995
if n_params(0) eq 0 then begin
 print,'wrtfitspec,h0,h1,h2,cts,gti,wmap,specname=specname,hdr,hdr1,hdr2'
 print,'Write a FITS spectral file given the 1-D array CTS'
 print,'which contains COUNTS. The Headers h0,h1,h2 are created'
 print,'with the routine MKSPECHDR. The output file is SPECNAME'
 retall
end
if n_elements(specname) eq 0 then begin
 specname=' '
 read,'Enter name of output spectral file ',specname
endif
;write the primary array and header
fxwrite,specname,h0,wmap
npha = (size(cts))(1)+0l
;create the 1st bit of the extension header
fxbhmake,hdr1,npha,'SPECTRUM',' name of this binary table extension'
nh=where(strmid(hdr1,0,8) eq 'END     ',nend)
;print,'nh ',nh(0)
nh(0)=15
;create the columns
chan=1l+lindgen(npha)
qual=lonarr(npha)
fxbaddcol,col1,hdr1,chan(0),'CHANNEL'
fxbaddcol,col2,hdr1,cts(0),'COUNTS'
fxbaddcol,col3,hdr1,qual(0),'QUALITY'
;hdr1=hdr1(0:nh(0)-1)
;print,'1st head ',(size(hdr1))(1)
;print,'2nd head ',(size(h1))(1)
hdr=[hdr1(0:nh(0)-1),h1]
;print,'3rd head ',(size(hdr))(1)
;write the header for the 1st extension
fxbcreate,unit,specname,hdr
;write the data
for i=1l,npha do begin
 fxbwrite,unit,chan(i-1),col1,i
 fxbwrite,unit,cts(i-1),col2,i
 fxbwrite,unit,qual(i-1),col3,i
endfor
fxbfinish,unit
;now append the GTI extension
;create the 1st bit of the 2nd extension header
ngti=(size(gti))(1)+0l
fxbhmake,hdr2,ngti,'GTI     ',' name of this binary table extension'
tstart=gti(0:ngti-1,0)+0.0d0 & tstop=gti(0:ngti-1,1)+0.0d0
fxbaddcol,col4,hdr2,tstart(0),'START',tunit='s'
fxbaddcol,col5,hdr2,tstop(0),'STOP',tunit='s'
nh(0)=15
hdr=[hdr2(0:nh(0)-1),h2]
fxbcreate,unit2,specname,hdr
;write the data
for i=1l,ngti do begin
 fxbwrite,unit2,tstart(i-1:i-1),col4,i
 fxbwrite,unit2,tstop(i-1:i-1),col5,i
endfor
fxbfinish,unit2
return
end
