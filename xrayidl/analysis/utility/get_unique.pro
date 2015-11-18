pro get_unique,loc,indsrt,ldup,kdup
;+
; calculate location and duplication of unique values in a vector
; loc - input vector 
; indsrt - sorted index of the vector
; Ldup,Kdup - index location and number of duplication of the unique values
;
; extracted from calcl.pro. wqd, 7/13/96
;-
indsrt = sort(Loc)                     
Loc = [ Loc(indsrt), -1 ]
Ldup = where ( Loc(1:*)-Loc )
if N_elements( Ldup ) EQ 1 then $
   Kdup = Ldup(0)+1         $
   else Kdup = [ Ldup(0)+1 , Ldup(1:*)-Ldup ]      
return
end