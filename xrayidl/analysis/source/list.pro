pro list,infile,outfile,fch=fch
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - list,infile,outfile,fch=fch'
return
endif
star_number=1
ra_hour=1
ra_min=1
ra_sec=1.
dec_deg=1
dec_min=1
dec_sec=1.
source_sn=1.
cntr=1.
x_core=1
y_core=1

openr,un,infile,/get
openw,unout,outfile,/get
if n_elements(fch) eq 0 then fch=0
case fch of 
	0: begin 
		formin='(I3, 2(2i4, f7.2), f9.2, f11.5,2I4)'
		formout='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(I4,a2))'
	   end
	1: begin
		formin='(I3,2x,2(2i4, f7.2,2x), f9.2,2x, f9.5,2x,2(f8.2,2x),f8.1,2x,f9.1,2x)'
		formout='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f9.5,a2,2(f8.2,a2),f8.1,a2,f9.1,a2)'
	   end
endcase
while not eof(un) do begin
	 readf,un, star_number, ra_hour,ra_min,ra_sec $
	 ,dec_deg,dec_min,dec_sec, source_sn, cntr, $
	 x_core,y_core,format=formin

	 printf,unout, star_number,' |', ra_hour,ra_min $
	,ra_sec,' |',dec_deg,dec_min,dec_sec,' |',source_sn, $
	' |',cntr,' |', x_core,' |',y_core,' |' $
	,format=formout
endwhile
free_lun,un
free_lun,unout
end
