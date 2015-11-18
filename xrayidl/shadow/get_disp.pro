pro get_disp,dv,mv,disp
common flux,dvc,mvc
dvc=dv
mvc=mv

bbi=[1.,0.,2.]
func='linear'
;bb=broyden(bb,func)
bbi=[1.]
bb=newton(bbi,func)
;func='test'
;bbi=[0.,-!pi/2,!pi]
bb=fx_root(bbi,func,/double)
stop
aa=sqrt(avg((dv-mv)^2/(1.+bb*mv)^2))
disp=aa(1.+bb*mv)
print,aa,bb
stop
return
end

function linear, bb
common flux,dvc,mvc
nbb=n_elements(bb)
cc=dblarr(nbb)
dif2=(dvc-mvc)^2
for k=0,nbb-1 do begin
	dn=1.+bb(k)*mvc
	cc(k)=avg(dif2/dn)*total(mvc/dn)-total(dif2*mvc/dn^2)
endfor
if nbb eq 1 then cc=cc(0)
print,cc
return, cc
end

function test, bb
return, exp(sin(bb)^2+cos(bb)^2-1)-1
end