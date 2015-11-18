pro msel,sel,v0,v1,v2,v3,v4,v5,v6,v7,v8,v9
;+
; sel elements from multiple arrays
; written by wqd, 12/6/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - msel,sel,v0,v1,v2,v3,v4,v5,v6,v7,v8,v9'
return
endif

nv=n_params()-1
case nv of 
0: begin
	print,'variables need to be given'
	return
	end
1: v0=v0(sel)
2: begin
	v0=v0(sel)
	v1=v1(sel)
	end
3: begin
	v0=v0(sel)
	v1=v1(sel)
	v2=v2(sel)
	end
4: begin
	v0=v0(sel)
	v1=v1(sel)
	v2=v2(sel)
	v3=v3(sel)
	end
5: begin
	v0=v0(sel)
	v1=v1(sel)
	v2=v2(sel)
	v3=v3(sel)
	v4=v4(sel)
	end
6: begin
	v0=v0(sel)
	v1=v1(sel)
	v2=v2(sel)
	v3=v3(sel)
	v4=v4(sel)
	v5=v5(sel)
	end
7: begin
	v0=v0(sel)
	v1=v1(sel)
	v2=v2(sel)
	v3=v3(sel)
	v4=v4(sel)
	v5=v5(sel)
	v6=v6(sel)
	end
8: begin
	v0=v0(sel)
	v1=v1(sel)
	v2=v2(sel)
	v3=v3(sel)
	v4=v4(sel)
	v5=v5(sel)
	v6=v6(sel)
	v7=v7(sel)
	end
9: begin
	v0=v0(sel)
	v1=v1(sel)
	v2=v2(sel)
	v3=v3(sel)
	v4=v4(sel)
	v5=v5(sel)
	v6=v6(sel)
	v7=v7(sel)
	v8=v8(sel)
	end
10: begin
	v0=v0(sel)
	v1=v1(sel)
	v2=v2(sel)
	v3=v3(sel)
	v4=v4(sel)
	v5=v5(sel)
	v6=v6(sel)
	v7=v7(sel)
	v8=v8(sel)
	v9=v9(sel)
	end
endcase
return
end