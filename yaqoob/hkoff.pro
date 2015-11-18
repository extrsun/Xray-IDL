pro hkoff,hkinfo,hk,off,hkn
;Author T. Yaqoob - March 1993 ->**
 if n_params(0) eq 0 then begin
  print,'hkoff,hkinfo,hk,off,hkn'
  print,'turn on/off the hkn th hk parameter (counting from 1)'
  print,' OFF: <-1 ON: =FOV or -1 (don t care about FOV)'
  retall
end
hkn=hkn-1
if hkn lt 0 then return
flags=hk.flag
flags(hkn)=off
hk.flag=flags
return
end
