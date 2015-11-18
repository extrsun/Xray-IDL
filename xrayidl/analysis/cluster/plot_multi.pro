pro plot_multi,xlo,xhi,ylo,yhi,nx,ny,ximc,yimc,index

ximc=xlo+(xhi-xlo)*indgen(nx+1)/nx
yimc=ylo+(yhi-ylo)*indgen(ny+1)/ny
index_conv,indgen(nx*ny),[nx,ny],index
return
end
	