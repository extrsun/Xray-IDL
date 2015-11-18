pro exp_comb,outfhead,ta,pn_head=pn_head,mos1_head=mos1_head,mos2_head=mos2_head,bv=bv
;+
; create combined MOS1+MOS2+pn exposure maps in individual bands
;
; outfhead - the head name of the output maps (e.g., 'pn2mos')
; ta - output array of exposure maps in individual (bv) selected bands,
;		normalized to the value at the aiming point
; bv - selecting vector containing band numbers (def =[1,2,3,4])
; *_head - the head name of pn, MOS1 and MOS2 exposure map files, to
;         be added to '_i?.fits'; (def = 'pn', 'mos1', and 'mos2')
;
; written by wqd, 3/22/2007
;
;-
if n_params() eq 0 then begin
print,'Calling Seq. - exp_comb,outfhead,ta,pn_head=pn_head,mos1_head=mos1_head,mos2_head=mos2_head,bv=bv'
return
endif
if n_elements(pn_head) eq 0 then pn_head='pn'
if n_elements(mos1_head) eq 0 then pn_head='mos1'
if n_elements(mos2_head) eq 0 then pn_head='mos2'
if n_elements(bv) eq 0 then bv=[1,2,3,4]

;from XSPEC for the bands (0.5_1.0,1.0_2.0,2.0_4.5,4.5_7.5) keV:
pn=[14.99,184.7,204.0,86.89]
mos1=[4.036,62.95,76.03,23.87]
mos2=[3.983,62.67,77.06,25.39]
mos1=mos1/pn
mos2=mos2/pn

nbt=n_elements(bv)
ta=fltarr(nbt)
for k=0,nbt-1 do begin
    infile=pn_head+'_i'+strtrim(bv(k),2)+'.fits'
    tb=readfits(infile,mh)
    infile=mos1_head+'_i'+strtrim(bv(k),2)+'.fits'
    tb=tb+readfits(infile)*mos1(k)
    infile=mos2_head+'_i'+strtrim(bv(k),2)+'.fits'
    tb=tb+readfits(infile)*mos2(k)
    writefits,outfhead,tb,mh
    ta(*,*,k)=tb
endfor
end
