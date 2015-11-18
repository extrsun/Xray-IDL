pro detect_params,dis,core_size,ann_in,ann_out,acore_size=acore_size $
,aann_in=aann_in,aann_out=aann_out
;
acore_size=0.75
;acore_size=0.5
aann_out=4. ;in units of acore_size
aann_in=1.5 ;in units of acore_size
;aann_out=3. ;in units of acore_size
slope=0.03333
core_size=acore_size+slope*dis
ann_in=aann_in*core_size
ann_out=aann_out*core_size
;
end