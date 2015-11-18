pro read_objects,infile,slist,row=row,outfile=outfile
;+
; convert an ascii source file into a fits file
; row is not used yet.
;
; a modifield version sou_fits.pro
; written by wqd, 10/31/2003
;-
if n_params() eq 0 then begin
print,'CALLING Seq. - read_objects,infile,slist,row=row,outfile=outfile'
return
endif
if n_elements(row) eq 0 then $  ;row={sn:0,ra:0.0D0,dec:0.0D0}
;row={sn:'0',ra:0.0D0,dec:0.0D0,dist:0.0,peak:0.0,total:0.0,etotal:0.0,size:'0',z:0.0}
row={sn:'0',ra:0.0D0,dec:0.0D0,dist:0.0,peak:0.0,total:0.0,etotal:0.0,size:'0',z:0.0}
tagin=tag_names(row)

openr,un,infile,/get
str=''
text=strarr(10000)
k=0
while not eof(un) do begin
	readf,un,str
	text(k)=str
	k=k+1
endwhile
free_lun,un
nrow=k
text=text(0:nrow-1)
sym='&'
slist = replicate(row,nrow)
slist.sn=mgettok(text,sym)
;slist.snr=mgettok(text,sym,nsym=3)
ra=mgettok(text,sym)
ra=mgettok(ra,'(')
dec=mgettok(text,sym)
dec=mgettok(dec,'(')
radec=ra+' '+dec
for k=0,nrow-1 do begin
	stringad,radec(k),ras,decs
	ra(k)=ras
	dec(k)=decs
endfor
slist.ra=ra
slist.dec=dec
slist.dist=mgettok(text,sym)
slist.peak=mgettok(text,sym)
total=mgettok(text,sym)
slist.total=mgettok(total,'(')
slist.etotal=mgettok(total,')')
slist.size=mgettok(text,sym)
slist.z=mgettok(text,sym)

; output the combined source structure:
if n_elements(outfile) ne 0 then begin
    comm='converted from '+infile
    sou_struct_fits,slist,outfile,comm=comm
endif
return
end
