pro sou_fits,infile,outfile,row=row,hdrref=hdrref
;+
; convert an ascii source file into a fits file
; row is not used yet.
; written by wqd, 12/29/2001
;-
if n_params() eq 0 then begin
print,'CALLING Seq. - sou_fits,infile,outfile,row=row,hdrref=hdrref'
return
endif
if n_elements(row) eq 0 then $ ;row={sn:0,ra:0.0D0,dec:0.0D0}
row={sn:0,ra:0.0D0,dec:0.0D0,perr:0.0,snr:0.0,cntr:0.0,cntre:0.0,cntrb1:0.0,cntrb1e:0.0,cntrb2:0.0,cntrb2e:0.0,cntrb3:0.0,cntrb3e:0.0,mcntr:0.0,offaxis:0.0}
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

slist = replicate(row,nrow)
slist.sn=mgettok(text,'|')
slist.snr=mgettok(text,'|',nsym=3)
slist.cntrb=mgettok(text,'|')
slist.cntrbe=mgettok(text,'|')
slist.cntrb1=mgettok(text,'|')
slist.cntrb1e=mgettok(text,'|')
slist.cntrb2=mgettok(text,'|')
slist.cntrb2e=mgettok(text,'|')
slist.cntrb3=mgettok(text,'|')
slist.cntrb3e=mgettok(text,'|')
slist.offaxis=mgettok(text,'|')
slist.mcntr=mgettok(text,'&')
slist.ra=mgettok(text,'&')
slist.dec=mgettok(text,'&')
slist.perr=mgettok(text,'&')

; output the combined source structure:
comm='converted from '+infile
sou_struct_fits,slist,outfile,hdrref=hdrref,comm=comm
return
end
