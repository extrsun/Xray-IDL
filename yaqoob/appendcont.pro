pro appendcont,fname3,nstep3,x3,y3,dx3,dy3,lev3,x3cen,y3cen,cdata,x3a,y3a
if n_params(0) eq 0 then begin
 print,'appendcont,fname3,nstep3,x3,y3,dx3,dy3,lev3,x3cen,y3cen,cdata,x3a,y3a'
 print,'Intended to be used right after COMBCONT to produce combined '
 print,'contour plots of three or more sets of contours'
 print,'RUN combcont with the plot device set to ps and then run '
 print,'appendcont and plot over with '
 print,'contour,cdata,levels=lev3,x3a,y3a,/over,c_linest=1'
 print,'repeat until done'
 retall
end
ns3=nstep3+1
data = datxfer(fname3,ns3,0)
nx3 = (size(data))(2)
cdata = transpose(data) 
x3a=findgen(nx3)*dx3+x3
y3a=findgen(ns3)*dy3+y3
return
end
