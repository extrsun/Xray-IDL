pro wrt_fitspec,h0,h1,h2,cts,ctse,gti,wmap,specname=specname,hdr,hdr1,hdr2,qual=qual
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
;npha = (size(cts))(1)+0l
npha = n_elements(cts)
if n_elements(qual) eq 0 then qual=intarr(npha)

;create the 1st bit of the extension header
fxbhmake,hdr1,npha,'SPECTRUM',' name of this binary table extension'
;nh=where(strmid(hdr1,0,8) eq 'END     ',nend)
;print,'nh ',nh(0)
;nh(0)=18
;nh(0)=15
;create the columns
chan=1l+lindgen(npha)
fxbaddcol,col1,hdr1,chan(0),'CHANNEL'
fxbaddcol,col2,hdr1,cts(0),'COUNTS/S'
fxbaddcol,col3,hdr1,ctse(0),'COUNTS/S ERR'
fxbaddcol,col4,hdr1,qual(0),'QUALITY'
nh=where(strmid(hdr1,0,8) eq 'END     ',nend)
hdr=[hdr1(0:nh(0)-1),h1,'END     ']

;print,'3rd head ',(size(hdr))(1)
;write the header for the 1st extension
fxbcreate,unit,specname,hdr
;write the data
for i=1l,npha do begin
 fxbwrite,unit,chan(i-1),col1,i
 fxbwrite,unit,cts(i-1),col2,i
 fxbwrite,unit,ctse(i-1),col3,i
 fxbwrite,unit,qual(i-1),col4,i
endfor

fxbfinish,unit
;now append the GTI extension
;create the 1st bit of the 2nd extension header
ngti=(size(gti))(1)+0l
fxbhmake,hdr2,ngti,'GTI     ',' name of this binary table extension'
tstart=gti(0:ngti-1,0)+0.0d0 & tstop=gti(0:ngti-1,1)+0.0d0
fxbaddcol,col4,hdr2,tstart(0),'START',tunit='s'
fxbaddcol,col5,hdr2,tstop(0),'STOP',tunit='s'
;nh(0)=17
;hdr=[hdr2(0:nh(0)-1),h2]
nh=where(strmid(hdr2,0,8) eq 'END     ',nend)
hdr=[hdr2(0:nh(0)-1),h2,'END     ']
fxbcreate,unit2,specname,hdr
;write the data
for i=1l,ngti do begin
 fxbwrite,unit2,tstart(i-1:i-1),col4,i
 fxbwrite,unit2,tstop(i-1:i-1),col5,i
endfor
fxbfinish,unit2
return
end
