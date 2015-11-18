
.run image_get
.run image_comp
.run source_para
.run source_sub
.run image_cut
.run image_av
;
;if n_elements(seq_no) eq 0 then seq_no=!seq_no
seq_no=900175
print,seq_no
image_get,map_c,h,sn=strtrim(seq_no)+'_im1'
image_get,map_t,h,sn=strtrim(seq_no)+'_mex'
map_cc=image_comp(map_c,0.25)
map_tc=rebin(map_t,128,128)
;
crval=sxpar(h,'CRVAL*')/!radeg
source_para,seq_no,xs,ys
rs=xs*0.+120. 	
;if !debug eq 1 then stop			;arcsec
map_tcs=source_sub(map_tc,crval(0),crval(1),xs,ys,block=120,xdim=128, $
ydim=128)
ba=map_tc*1.e-4
maxbox=5
fluxerr=1.e-4
image_av,maxbox,fluxerr,128,128,map_cc,ba,map_tcs,f,map_tcsm
map_f=image_cut(f,45.,dim=128,block=120)
;map_tcsc=image_cut(map_tcs,45.,dim=128,block=120)
;map_ccc=image_cut(map_cc,45.,dim=128,block=120)
;map_f=imdiv(map_ccc,map_tcsc)*1.e4
map_fc=congrid(map_f,512,512,/interp)
tv,bscale(map_fc)
end



