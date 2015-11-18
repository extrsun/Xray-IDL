pro testloadct
filename='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/CRprecursor/4-6keV_bin1_truecolorb_sig3-5.fits'
image=mrdfits(filename,0,head)

window,1,retain=2
;for i=1,3 do begin
;for j=1,3 do begin
;for k=1,3 do begin
;loadct,10
device,decomposed=0
R=bytscl(sin(findgen(255)))
G=bytscl(sin(findgen(255)))
B=findgen(255)
tvlct,R,G,B,/GET
tvscl,dist(400)
;plot,[0,1],[0,1],background=i
;endfor
;endfor
;endfor
end
