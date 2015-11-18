pro OFeflux
data=[[4.72337E+10,1.059e-12,6.0591e-13,4.3022e-13],$
[4.72337E+10,1.059e-12,6.0591e-13,4.3022e-13],$
[5e9,9.2472e-13,4.1447e-13,3.9928e-13],$
[6e9,9.8906e-13,4.4369e-13,4.1695e-13],$
[7e9,1.0392e-12,4.7071e-13,4.3078e-13],$
[8e9,1.0788e-12,4.9509e-13,4.4157e-13],$
[9e9,1.1102e-12,5.1659e-13,4.4994e-13],$
[1e10,1.1350e-12,5.3520e-13,4.5637e-13],$
[2e10,1.1996e-12,6.1621e-13,4.7046e-13],$
[3e10,1.1578e-12,6.2334e-13,4.5725e-13],$
[4e10,1.1002e-12,6.1463e-13,4.4113e-13],$
[5e10,1.0438e-12,6.0244e-13,4.2631e-13],$
[6e10,9.9245e-13,5.8997e-13,4.1338e-13],$
[7e10,9.4660e-13,5.7813e-13,4.0216e-13],$
[8e10,9.0590e-13,5.6714e-13,3.9235e-13],$
[9e10,8.6977e-13,5.5700e-13,3.8370e-13],$
[1e11,8.3762e-13,5.4765e-13,3.7598e-13],$
[2e11,6.4701e-13,4.8316e-13,3.2682e-13]$
]

window,1,retain=2
outputjpg='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/work/OFeflux.jpg'
device,decomposed=0
plot,data[0,*],(data[1,*]-data[2,*])/(data[2,*]-data[3,*]),psym=4,color=0B,background=255B,/xlog,/ylog,xtitle='ionization parameter',ytitle='O flux/Fe flux'
oplot,data[0,0:1],(data[1,0:1]-data[2,0:1])/(data[2,0:1]-data[3,0:1]),psym=1,symsize=4,color=0B
device,decomposed=1

;imgarr=tvrd(true=1)
;write_jpeg,outputjpg,imgarr,quality=100,true=1

end
