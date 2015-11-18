pro makegeo,geo,det,inst
;Author T. Yaqoob - Oct 1995
if n_params(0) eq 0 then begin
 print,'makegeo,geo,det,inst'
 print,'Make a dummy GEO structure: this is required '
 print,'if your photon list was not read into IDL by READASCA'
 print,'Use SELREG to then covert the dummy GEO to one which '
 print,'carries the correct information. The minimum required '
 print,'parameters for the dummy GEO are: '
 print,'DET	- =0 for RAW coords, 1 for DETECTOR coords '
 print,'INST	- = 0 for SIS0 '
 print,'    	  = 1 for SIS1 '
 print,'	  = 2 for GIS2 '
 print,'	  = 3 for GIS3 '
 print,'	  = 4 for PSPC '
 retall
end
dum0=fltarr(1) & dum1=fltarr(2) & dum2=fltarr(5)
geo={geomet,type:0,cen1:dum1,cen2:dum1,d1:dum2,d2:dum2,s1:dum2,s2:dum2,$
inst:0,det:0,chp:0,bscl:dum0}
geo.det=det
geo.inst=inst
return
end
