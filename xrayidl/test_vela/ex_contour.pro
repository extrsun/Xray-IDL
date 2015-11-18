pro ex_contour

device, decomposed=0
data=hanning(400,400)
contour, data ,/nodata
xysize=convert_coord(!x.crange,!y.crange,/data,/to_device)
xsize=abs(xysize[0,1]-xysize[0,0])
ysize=abs(xysize[1,1]-xysize[1,0])
tvscl, congrid(data,xsize,ysize),!x.crange[0],!x.crange[0],/data
contour, data, /noerase
wshow

end
