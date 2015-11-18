pro median_circle,data,pm,plo,phi,siglevel=siglevel,pbest=pbest

avg_median,data,pm,siglevel=siglevel
if n_elements(pbest) eq 0 then pbest=pm
ndata=data-pbest
sel=where(ndata gt (!pi*0.5),nsel)
if nsel ne 0 then ndata(sel)=ndata(sel)-!pi
sel=where(ndata lt -(!pi*0.5),nsel)
if nsel ne 0 then ndata(sel)=ndata(sel)+!pi
avg_median,ndata,npm,nplo,nphi,siglevel=siglevel
plo=nplo+pbest
phi=nphi+pbest
return
end