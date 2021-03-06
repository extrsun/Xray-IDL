pro sou_var_mccd,soufile,backfile,gtifile,gtiextv,mask,ccd,lso,cra,cdec,emin,emax,tb,xmin,ymin,dim,block,sfrac=sfrac,psffile=psffile,slow=slow,rsfac=rsfac,outfile=outfile,probth=probth,tblimit=tblimit

nccd=n_elements(ccd)
for k=0,nccd-1 do begin
	sel=where(mask eq ccd(k),nsel)
	if nsel eq 0 then stop,'no bin for ccd # ',ccd(k)
	tb1=tb*0.
	tb1(sel)=tb(sel)
	ls1=lso(where(lso.ccd eq ccd(k)))
	gtiext=gtiextv(k)
	if k eq 0 then $
	 sou_var,soufile,backfile,gtifile,gtiext,ls1,cra,cdec,emin,emax,tb1 $
	 ,xmin,ymin,dim,block,sfrac=sfrac,psffile=psffile,slow=slow $
	 ,rsfac=rsfac,outfile=outfile,probth=probth,tblimit=tblimit $
	else begin
	 sou_var,soufile,backfile,gtifile,gtiext,ls1,cra,cdec,emin,emax,tb1 $
	 ,xmin,ymin,dim,block,sfrac=sfrac,psffile=psffile,slow=slow $
	 ,rsfac=rsfac,outfile=outfile,probth=probth,tblimit=tblimit $
	 ,/append
	endelse
endfor
return
end
