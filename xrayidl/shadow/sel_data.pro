pro sel_data,npb,ba,et,sel,band,count,back,time
	cc=float(npb(*,*,band)) & count=cc(sel)
	cc=ba(*,*,band) & back=cc(sel)
	cc=et(*,*,band) & time=cc(sel)
end