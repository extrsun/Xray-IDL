pro pixel_radec,imcra,imcdec,xp,yp,ra,dec
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - pixel_radec,imcra,imcdec,xp,yp,ra,dec'
return
endif
if !instr eq 'h' then begin
xdis=xp-4096.5 ;assuming the FORTRAN coordinates
ydis=yp-4096.5 ; for HRI images only; reproducing the RA and DEC to 0.0005
	     ; in the *_src table; 0.0005 is the accuracy of RA and Dec.
endif else begin 
xdis=xp-7680.5 ;assuming the FORTRAN coordinates
ydis=yp-7680.5 ; for PSPC images only; reproducing the RA and DEC to 0.0005
	     ; in the *_src table; 0.0005 is the accuracy of RA and Dec.
endelse 
trans_loct,xdis,ydis,imcra,imcdec,ra,dec,/deg
end

