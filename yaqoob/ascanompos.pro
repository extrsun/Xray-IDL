pro ascanompos,human
if n_params(0) eq 0 then begin
 print,'ascanompos,human'
 print,'Print Coords of Nominal Source Position for ASCA CCD modes'
 print,'HUMAN 	:=0  Look down animal coords (jbldarf-0.45)'
 print,'	:=1  Human Look Up Coords '
 retall
end
npos=4
;instrument centres in satellite coords 
instcen=fltarr(4,2)
instcen(0,0)=0.0 & instcen(0,1)=0.0
instcen(1,0)=0.06 & instcen(1,1)=0.97
instcen(2,0)=0.76 & instcen(2,1)=0.86
instcen(3,0)=-0.75 & instcen(3,1)=1.01
;opical axes in Sat coords
optic=fltarr(4,2)
optic(0,0)=0.6 & optic(0,1)=2.2
optic(1,0)=-0.6 & optic(1,1)=-3.6
;optic(2,0)=1.125 & optic(2,1)=-0.615
;following is from XRT status report Feb 1994 
optic(2,0)=1.000 & optic(2,1)=-0.49
;optic(3,0)=-2.285 & optic(3,1)=-1.485
optic(3,0)=-2.41 & optic(3,1)=-1.36
;operations centre in sat coords
opcen=[-0.58,1.31]
;nominal positions relative to operations centre
nompos=fltarr(npos,2)
nompos(0,0)=-5.0 & nompos(0,1)=5.0
nompos(1,0)=-3.5 & nompos(1,1)=3.25
nompos(2,0)=0.0 & nompos(2,1)=5.0
nompos(3,0)=0.0 & nompos(3,1)=1.0 
nomname=strarr(npos)
nomname(0)='Old 1CCD '
nomname(1)='New 1CCD '
nomname(2)='    2CCD '
nomname(3)='    4CCD '
nom=fltarr(npos,4,2)
for i=0,npos-1 do begin
 for j=0,3 do begin
  nom(i,j,0)=nompos(i,0)+opcen(0)-instcen(j,0)
  nom(i,j,1)=nompos(i,1)+opcen(1)-instcen(j,1)
 endfor
endfor
axes=optic
axes(0:3,1:1)=-1.*optic(0:3,1:1)
nom(0:npos-1,0:3,1:1)=-1.*nom(0:npos-1,0:3,1:1) 
;For Umans
sispx=0.027 & gispx=0.25
if human gt 0 then begin
 nom(0:npos-1,0:1,0:1)=(nom(0:npos-1,0:1,0:1)/sispx)+640.5
 nom(0:npos-1,2:3,0:1)=(nom(0:npos-1,2:3,0:1)/gispx)+128.5
endif
print,'CCD POS	    	S0  		S1		S2		S3'
if human eq 0 then begin
for i=0,npos-1 do begin
 print,format='(A,5X,4(F5.2,1X,F5.2,5X))',nomname(i),nom(i,0,0),nom(i,0,1),$
 nom(i,1,0),nom(i,1,1),nom(i,2,0),nom(i,2,1),nom(i,3,0),nom(i,3,1)
endfor
 print,format='(A,5X,4(F5.2,1X,F5.2,5X))','Optical axes ',axes(0,0),$
axes(0,1),axes(1,0),axes(1,1),axes(2,0),axes(2,1),axes(3,0),axes(3,1)
endif
if human gt 0 then begin
 for i=0,npos-1 do begin
print,format='(A,1X,4(F6.1,1X,F6.1,4X))',nomname(i),nom(i,0,0),nom(i,0,1),$
nom(i,1,0),nom(i,1,1),nom(i,2,0),nom(i,2,1),nom(i,3,0),nom(i,3,1)
 endfor
; axes(0:3,1:1)=-1.*axes(0:3,1:1)
 axes(0:1,0:1)=(axes(0:1,0:1)/sispx)+640.5
 axes(2:3,0:1)=(axes(2:3,0:1)/gispx)+128.5
 print,format='(A,1X,4(F6.1,1X,F6.1,4X))','Optical axes ',axes(0,0),$
axes(0,1),axes(1,0),axes(1,1),axes(2,0),axes(2,1),axes(3,0),axes(3,1)
endif
return
end
