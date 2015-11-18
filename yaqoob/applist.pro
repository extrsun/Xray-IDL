pro applist,sublist,pi,clo,chi,master
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin
 print,'applist,sublist,pi,clo,chi,master'
 print,'Program appends a photon list onto another even if they '
 print,'events are from different instruments '
 print,'If master does not alreay exist it is created '
 print,'The events which are actually appended will only have PHA/PI'
 print,'values in the range clo-chi inclusive. You specify whether '
 print,'the selection is on PHA or PI by the the parameter PI '
 print,'i.e. PI=0 means PHA and PI=1 means PI '
 retall
end
;set up the format of the new photon list
row={unieve,x:0,y:0,detx:0,dety:0,pha:0,pi:0,time:0.0d0}
;what's the size of sublist?
nsub=(size(sublist))(1)
print,'Number of events in sublist ',nsub
if pi eq 0 then begin
 qlist=sublist(where(sublist.pha ge clo and sublist.pha le chi))
endif
if pi eq 1 then begin
 qlist=sublist(where(sublist.pi ge clo and sublist.pi le chi))
endif
nq=(size(qlist))(1)
print,'Events which satisfy the PHA/PI condition ',nq
unilist=replicate(row,nq)
unilist.x=qlist.x & unilist.y=qlist.y & unilist.detx=qlist.detx
unilist.dety=qlist.dety & unilist.pha=qlist.pha
unilist.pi=qlist.pi & unilist.time=qlist.time
;is there a master?
if n_elements(master) eq 0 then begin
 print,'Master list does not exist - creating one '
 master=unilist
 goto, fin
end
nmas=(size(master))(1)
print,'Number of events in Master list ',nmas
print,'Appending sublist onto master ' 
master=[master,unilist]
np=(size(master))(1)
print,'Total number of events in Master now ',np
fin: return
end
