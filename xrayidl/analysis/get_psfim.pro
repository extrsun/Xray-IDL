pro get_psfim,psfim,dim=dim,block=block,chlow=chlow,chhigh=chhigh $
,binrat=binrat,offaxis=offaxis
;+
;get a PSPC point spread funtion image 
;
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_psfim,psfim,dim=dim,block=block'
print,',chlow=chlow,chhigh=chhigh,binrat=binrat,offaxis=offaxis   '
retall
endif
if n_elements(offaxis) eq 0 then offaxis=0.
if n_elements(dim) eq 0 then dim=361
if n_elements(block) eq 0 then block=2
if n_elements(chlow) eq 0 then chlow=12
if n_elements(chhigh) eq 0 then chhigh=29
if n_elements(binrat) eq 0 then binrat=5
;
filen='~/spectrum/po17.dat'
;
modelrate,filen,chlow,chhigh,rate,group
;
print,'get point spread function ...'
binsize=0.5*block
calcpsfp,dim,binsize,offaxis,rate,offang,prof,psfim,group=group,binrat=binrat $
,chatt=0
;
return
end