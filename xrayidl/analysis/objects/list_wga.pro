pro list_wga,infile,outfile,fch=fch
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - list,infile,outfile,fch=fch'
return
endif
star_number=0
ra_hour=1
ra_min=1
ra_sec=1.
dec_deg=1
dec_min=1
dec_sec=1
source_sn=1
cntr=1.
x_core=1
y_core=1

openr,un,infile,/get
openw,unout,outfile,/get
;formin='(5x,a12,2I3,f6.2,3I3,4x,i2,f9.4)'
formin='(17x,2I3,f6.2,3I3,4x,i2,f9.4)'
formout='(a12,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2)'
while not eof(un) do begin
;	 readf,un, star_number, ra_hour,ra_min,ra_sec $
	 readf,un, ra_hour,ra_min,ra_sec $
	 ,dec_deg,dec_min,dec_sec, source_sn, cntr,format=formin
	star_number=star_number+1
	 printf,unout, star_number,' |', ra_hour,ra_min $
	,ra_sec,' |',dec_deg,dec_min,dec_sec,' |',source_sn, $
	' |',cntr,' |'	,format=formout
endwhile
free_lun,un
free_lun,unout
end
