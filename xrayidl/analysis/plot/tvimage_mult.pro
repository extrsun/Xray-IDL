map_cc=image_comp(map_c,0.5)
map_tc=rebin(map_t,256,256)
c=where(map_t ne 0,count) 
flux_a=total(map_c)/count
tv,bscale(map_cc,0.,12.*flux_a),0
;
if!debug eq 1 then stop
map_fc=congrid(f,256,256,/interp)
tv,bscale(map_fc),1
tvcircle,45.*2,127.5,127.5
;
if!debug eq 1 then stop
tv,bscale(map_tc),2
tv,bscale(rebin(map_tcs,256,256)),3
color=1
thick=2
source_tv,crval(0),crval(1),127.5,383.5,xs,ys,block=60,color=color,thick=thick
source_tv,crval(0),crval(1),383.5,383.5,xs,ys,block=60,color=color,thick=thick
source_tv,crval(0),crval(1),127.5,127.5,xs,ys,block=60,color=color
source_tv,crval(0),crval(1),383.5,127.5,xs,ys,block=60,color=color
end
