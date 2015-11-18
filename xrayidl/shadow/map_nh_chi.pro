pro map_nh_chi,sel,nhd,npb,ba,et,fitband,chiband,xfluxfixlo,xfluxfixhi,ninc, $
	chi,opfile=opfile,nhdecmin=nhdecmin
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - map_nh_chi,sel,nhd,npb,ba,et,fitband,chiband'
print,',xfluxfixlo,xfluxfixhi,ninc,chi,opfile=opfile,nhdecmin=nhdecmin'
return
endif

chi=fltarr(ninc(0)+1,ninc(1)+1)
df=(xfluxfixhi-xfluxfixlo)/ninc
flux0=xfluxfixlo(0)+findgen(ninc(0)+1)*df(0)
flux1=xfluxfixlo(1)+findgen(ninc(1)+1)*df(1)
for j=0,ninc(1) do begin
	for i=0,ninc(0) do begin
		xfluxfix=[flux0(i),flux1(j)]
		map_nh,sel,nhd,npb,ba,et,nhn,minxflux,fitband=fitband $
		,chiband=chiband,maxint=maxint,opfile=opfile, $
		nval=nval,chimin=chimin,xfluxfix=xfluxfix,nhdecmin=nhdecmin
		chi(i,j)=chimin
		print,'i,j,chimin= ',i,j,chimin
	endfor
endfor
stop
end