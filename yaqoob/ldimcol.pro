pro ldimcol,slope
;load color table for tv
r=bytarr(256) & g=r & b=r
g=slope*bindgen(256) < 255
b=slope*bindgen(256)/4 < 255
r=slope*bindgen(256)/16 < 255
tvlct,r,g,b
return
end
