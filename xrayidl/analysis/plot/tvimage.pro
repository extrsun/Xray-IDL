pro tvimage,image_c,h_c,image_t,h_t,sn=seq_no,dir=dir,uppflux=fm
;
if n_elements(seq_no) eq 0 then seq_no=!seq_no else !seq_no=seq_no
if n_elements(dir) ne 0 then !data_dir=dir
if n_elements(fm) eq 0 then fm=10.
fname=seq_no+'_im1.fits'
image_c=readfits(fname,h_c)
fname=seq_no+'_mex.fits'
image_t=readfits(fname,h_t)
;
if !silence eq 0 then begin
c=where(image_t ne 0,count) 
flux_a=total(image_c)/count
tv,bscale(image_c,0.,10.*flux_a)
endif
;
end
