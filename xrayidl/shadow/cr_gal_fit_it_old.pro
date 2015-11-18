pro cr_gal_fit_it,dv,nhv,ma,ttv,ne2,fe,chi,selo=selo,mbest=mbest,opfile=opfile,imd=imdo,rlo=rlo,rd=rd,seln=seln
if n_elements(rlo) eq 0 then rlo=0.3
if n_elements(rd) eq 0 then rd=0.9
if n_elements(mit) eq 0 then mit=50
;if n_elements(selo) eq 0 then selo=lindgen(n_elements(dv))
seln=selo
imd=imdo
filter=imd*0.+1.
ss=lindgen(n_elements(seln))
ratioth=rd
for k=1,mit do begin
 nrsel=1
 while nrsel ne 0 do begin
	cr_gal_fit,dv(seln),nhv(seln),ma(ss,*),ttv,ne2,fe,chi $
		,mbest=mbest,opfile=opfile
	imm=imd*0.
	imm(seln)=mbest
	ratio=imdiv(imd-imm,imm)
	rsel=where(abs(ratio) gt ratioth,nrsel) 
	if nrsel ne 0 then begin
		imm(rsel)=0. & imd(rsel)=0. & filter(rsel)=0.
	endif
if !debug eq 1 then stop 
	ss=where(filter(selo) gt 0.)
	seln=selo(ss) 
 endwhile
 tv,bscale(imm,0.,5),k

 if ratioth lt rlo then stop,'want to change rlo?'
 if ratioth ge rlo then ratioth=ratioth*rd else goto,done
endfor
done:
return
end
	

