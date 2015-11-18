pro trans_coor,ra,dec
snr=['0 44 47.6 -73 24 19','0 45 26 -73 24 40', '0 46 34 -73 35 50', '0 47 17 -73 30 50',' 0 49 24 -73 37 40', '0 50 57 -72 53 30', '0 56 37 -72 33 34','0 57 44.9 -72 26 12', '0 58 42 -71 49 30', '1 1 38 -72 26 3', '1 2 24 -72 17 57', '1 3 34 -72 39 20','1 4 45 -72 21 30']
ns=n_elements(snr)
ra=fltarr(ns)
dec=ra
for i=0,ns-1 do begin
stringad,snr(i),cra,cdec
ra(i)=cra
dec(i)=cdec
endfor
precess,ra,dec,1950,2000 
trans_degree,ra/!radeg,dec/!radeg,ih,im,is,jd,jm,js 
for i=0, ns-1 do print,ih(i),im(i),is(i),jd(i),jm(i),js(i)
end
