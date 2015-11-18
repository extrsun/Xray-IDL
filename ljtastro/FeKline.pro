pro FeKline
flux=[[1.3953E-13,1.0385E-13,2.0377E-14,2.0211E-14],[3.1712E-13,2.2897E-13,5.6863E-14,5.1181E-14],$
[6.4241E-13,4.5500E-13,3.7521E-13,2.7230E-13],[1.7991E-12,1.5852E-12,6.0272E-13,5.7833E-13],$
[1.4330E-11,1.1523E-11,2.8329E-14,2.5415E-14]]
;0509-67.5,0519-69.0,N103B,N132D,Kepler

total=flux[0,*]
nonthermal=flux[1,*]-flux[2,*]
nonthermalFeK=flux[0,*]-flux[1,*]
thermalFeK=flux[2,*]-flux[3,*]

window,1,retain=2
outputjpg='/home/ljt/ljt/data/AboutSNR/SNR/FeKline/FeK.jpg'
plot,nonthermal/total,nonthermalFeK/(nonthermalFeK+thermalFeK),psym=2,/ynozero,xtitle="nonthermal flux/total flux (4-9keV band)"$
,ytitle="nonthermal FeK flux/total FeK flux (4-9keV band)"

;imgarr=tvrd(true=1)
;write_jpeg,outputjpg,imgarr,quality=100,true=1

end
