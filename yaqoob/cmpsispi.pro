pro cmpsispi,filename,dname=dname,fname=fname
if n_params(0) eq 0 then begin
 print,'cmpsispi,filename,dname=dname,fname=fname'
 print,'Take an SIS FITS eventfile [filename] with PI column already filled'
 print,'Then compute new PI values and compare these with the existing'
 print,'ones. The new values use the routine SISPHI'
 print,'DNAME//FNAME is the sisph2pi calibration file name'
 print,'eg dname=/home/astd1/yaqoob/siscal/ and fname=sisph2pi.fits'
 retall
end
tab=readfits(filename,h,ext=1)
brplist,1,h,tab,plist
inst=sxpar(h,'INSTRUME')
if strmid(inst,3,1) eq '0' then sistype=0 else sistype=1
slist=plist
print,'TIME range :',minmax(slist.time)
w0=where((plist.ccd eq 0),nw0)
w1=where((plist.ccd eq 1),nw1)
w2=where((plist.ccd eq 2),nw2)
w3=where((plist.ccd eq 3),nw3)
print,'Existing PI ranges: '
if nw0 gt 0 then print,'C0 PI ',minmax(plist(w0).pi)
if nw1 gt 0 then  print,'C1 PI ',minmax(plist(w1).pi)
if nw2 gt 0 then print,'C2 PI ',minmax(plist(w2).pi)
if nw3 gt 0 then print,'C3 PI ',minmax(plist(w3).pi)
sisphi,slist,sistype,dname=dname,fname=fname,fchp=0
print,'NEW PI ranges using SISPHI:'
if nw0 gt 0 then print,'C0 PI ',minmax(slist(w0).pi)
if nw1 gt 0 then print,'C1 PI ',minmax(slist(w1).pi)
if nw2 gt 0 then print,'C2 PI ',minmax(slist(w2).pi)
if nw3 gt 0 then print,'C3 PI ',minmax(slist(w3).pi)
return
end
