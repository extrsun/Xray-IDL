pro prf_cal,bandlow,bandhigh
;
dir='/home/casa/wqd/rosat/prf/'
;
file=['rp110595','rp110594','rp110602','rp110586','rp110590', $
   'rp110599','rp110591','rp110598']
tail='_acf'+strtrim(bandlow,2)+strtrim(bandhigh,2)+'.dat'
posi_sx=[199.311,237.89,256.065,288.28,230.99,318.00,254.53,299.59]
posi_sy=[236.857,198.55,256.10,205.68,312.42,259.155,316.61,297.524]
;
emin=!bandch(bandlow,0)
emax=!bandch(bandhigh,1)
print,'emin,emax = ',emin,emax
dim=61
length=15.
image_t=fltarr(dim,dim)+1.
;
for k=0,0 do begin
make_image,image,seq_no=file(k),dir=dir,dim=dim,xc=posi_sx(k),yc=posi_sy(k), $
emin=emin,emax=emax
;
outfile=file(k)+tail
get_acf,30.,image,image_t,length,angle,acf,acferr,nbin,outfile=outfile
endfor
;
end
