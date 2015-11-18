pro gtgrade,phas,split,echo,grade
;get event grade for faint mode phas array
;set up lookup table
;INPUTS phas - 9 pha values for each event
;	split - split threshold pha value
;	echo - correct for echo effect if non-zero
;	as of 11 jun 1993 this number is the fraction of the central
;	pixel(phas(0)) to be subtracted from the right side pixel
;	(phas(6))
;OUTPUTS 
;	 grade - grade classification for event type (range 0-7)
;	Kieth Gendreau reckons echo=0.0087 for S1 and 0.014 for S0
;ok first correct for echo effect
if (echo gt 0.0) then phas(5,*) = phas(5,*) - echo*phas(0,*)
look=intarr(256)
l1=[0,1,2,5,1,1,5,7,3,5,8,6,3,5,7,7]
l2=[4,4,8,7,5,5,6,7,7,7,7,7,7,7,7,7]
l3=[1,1,2,5,1,1,5,7,5,7,7,7,5,7,7,7]
l4=[4,4,8,7,5,5,6,7,7,7,7,7,7,7,7,7]
l5=[2,2,7,7,2,2,7,7,8,7,7,7,8,7,7,7]
l6=[8,8,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
l7=[5,5,7,7,5,5,7,7,6,7,7,7,6,7,7,7]
l8=[7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
l9=[1,1,2,5,1,1,5,7,3,5,8,6,3,5,7,7]
l10=[5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
l11=[1,1,2,5,1,1,5,7,5,7,7,7,5,7,7,7]
l12=[5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
l13=[5,5,7,7,5,5,7,7,7,7,7,7,7,7,7,7]
l14=[6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
l15=[7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
l16=[7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
look=[l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16]
power=intarr(9)
power=[0,1,2,4,8,16,32,64,128]
if (size(phas))(0) eq 1 then np = 1
if (size(phas))(0) gt 1 then np=(size(phas))(2)
pha = intarr(np)
grade=intarr(np)
;get the look-up index
for j=0l,np-1 do begin
	index = 0
	for i=1,8 do begin
		if (phas(i,j) ge split) then begin
		   index = index + power(i)
		endif
	endfor
grade(j) = look(index)
if (grade(j) eq 8) then grade(j) = 6
grade(j)=grade(j)+1 
endfor
return
end
