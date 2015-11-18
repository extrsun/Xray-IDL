pro unique_image,wlloc,indsrt,Ldup,Kdup,wlcalc
;+
; fill an image (wlcalc) with unique values calculated by get_unique
; wlloc - unique values
; indsrt - sorted index of the image
; Ldup,Kdup - index location and number of duplication of the unique values
; wlcalc - an initialized image to be filled
;
; wqd, 7/13/96
;-
  ii = 0                    
  n1 = ii                                
  n2 = n1+kdup(ii)-1 
  wlcalc(indsrt(n1:n2)) = wlloc(ii)    
  for ii=1,n_elements(Ldup)-1 do begin               
    n1 = Ldup(ii-1)+1 
    n2 = Ldup(ii-1)+kdup(ii)
    wlcalc(indsrt(n1:n2)) = wlloc(ii)  
  endfor
return
end