pro list_sou,ra,dec,infile=infile,outfile=outfile,sortl=sortl,xshift=xshift,yshift=yshift,ashift=ashift,slow=slow,flow=flow,fsel=fsel
;-
; read a standard SASS source file *_src.fits into a ASCII data file
; imcra, imcdec - ra and dec of the image center in units of deg
;	for calculating source positions using directly their pixel positions
;
; wqd, Nov 20, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_sou,ra,dec,infile=infile,outfile=outfile'
print,',sortl=sortl,xshift=xshift,yshift=yshift,ashift=ashift,slow=slow,flow=flow'
return
endif
if n_elements(slow) eq 0 then slow=0.
if n_elements(flow) eq 0 then flow=0.
if n_elements(infile) eq 0 then infile=!data_dir+'source.dat'
if n_elements(outfile) eq 0 then outfile=!data_dir+strtrim(!seq_no,2)+'_sou_sass.dat'
	openr,un,infile,/get
k=0
radec=''
cntrs=0.
time=0L
texts=''
ra=fltarr(1000)
dec=fltarr(1000)
cntr=fltarr(1000)
source_sn=fltarr(1000)
text=strarr(1000)
for n=0,10 do readf,un, texts
while not eof(un) do begin
	readf,un,format='(19x,a21,f9.4,i7,a20)',radec,cntrs,time,texts
	if n_elements(fsel) ne 0 then begin
	if strtrim(texts,2) ne fsel then goto,next
	endif
	stringad,radec,rad,decd
	ra(k)=rad
	dec(k)=decd
	cntr(k)=cntrs
	source_sn(k)=sqrt(time*cntrs)
	text(k)=texts
	k=k+1
	next:
endwhile
k=k-1
free_lun,un
;	shift_xya,x_core,y_core,xshift=xshift,yshift=yshift,ashift=ashift
	ra=ra(0:k-1)/!radeg
	dec=dec(0:k-1)/!radeg
	cntr=cntr(0:k-1)
	text=text(0:k-1)
	source_sn=source_sn(0:k-1)
	nsource=k

if keyword_set(sortl) ne 0 then begin
	so=sort(ra)
	star_number=lindgen(nsource)
	ra=ra(so)
	dec=dec(so)
	cntr=cntr(so)
	source_sn=source_sn(so)
	text=text(so)
	x_core=fltarr(nsource)
	y_core=fltarr(nsource)
endif
trans_degree,ra,dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec
if outfile eq 'none' then goto,done
stop,'stop: the output file is: ',outfile
openw,unout,outfile,/get

for k=0,nsource-1 do begin
	if source_sn(k) ge slow and cntr(k) gt flow  then begin
	 print, star_number(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
	' |',cntr(k) $ ;' |', x_core(k),' |',y_core(k)
	,' |',text(k) $
	,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,a20)' ;2(I8,a2),a20)'

	 printf,unout, star_number(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
	' |',cntr(k),' |', text(k) $
	,format='(i3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,a20)' ; 2(i8,a2),a20)'
	endif
endfor
free_lun,unout
done:
end
