pro bbxrtlist,h1,tab1,slist
if n_params(0) eq 0 then begin
 print,'bbxrtlist,h1,tab1,slist'
 print,'make a photon list from bbxrt events'
 retall
end
;fix the headers first
hdum=h1(9) & strput,hdum,'1D',11 & h1(9)=hdum
hdum=h1(12) & strput,hdum,'1I',11 & h1(12)=hdum
hdum=h1(14) & strput,hdum,'1I',11 & h1(14)=hdum
hdum=h1(16) & strput,hdum,'1B',11 & h1(16)=hdum
hdum=h1(18) & strput,hdum,'1B',11 & h1(18)=hdum
hdum=h1(20) & strput,hdum,'1B',11 & h1(20)=hdum
hdum=h1(22) & strput,hdum,'1B',11 & h1(22)=hdum
hdum=h1(24) & strput,hdum,'1B',11 & h1(24)=hdum
time=tbget(h1,tab1,'time') & pha=tbget(h1,tab1,'phas')
led=tbget(h1,tab1,'led') & det=tbget(h1,tab1,'detector')
guard=tbget(h1,tab1,'guard') & vle=tbget(h1,tab1,'vle')
ptp=tbget(h1,tab1,'ptp') & ppu=tbget(h1,tab1,'ppu')
neve=(size(time))(1)
row={bevent,det:0,time:0.0d0,pha:0,led:0,guard:0,vle:0,ptp:0,ppu:0}
slist=replicate(row,neve)
slist.det=det & slist.time=time & slist.pha=pha & slist.led=led
slist.guard=guard & slist.vle=vle & slist.ptp=ptp & slist.ppu=ppu
return
end
