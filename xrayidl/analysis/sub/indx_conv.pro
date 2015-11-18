pro index_conv,indexo,dim,indexv
sz=size(dim)
index=indexo
indexv=[-999]
frac=1L
if sz(0) eq 1 then return
for k=1,sz(0)-1 do begin
	frac=frac*sz(k)
	indexv=[indexv,index mod frac]
	index=index/frac
endfor
indexv=indexv(1:*)
end
