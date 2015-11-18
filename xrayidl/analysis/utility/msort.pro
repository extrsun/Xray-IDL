pro msort,sv,v1,v2, v3, v4, v5, v6, v7, v8, v9
;+
; sort multiple variables according the first variable
; written by wqd, 12/27/2001
;-
nitems=n_params()-1
if nitems eq -1 then begin
print,'CALLING SEQUENCE - msort,sv,v1,v2, v3, v4, v5, v6, v7, v8, v9'
return
endif
ss=sort(sv)
sv=sv(ss)
case nitems of 
        1: v1=v1(ss)
        2: begin
	   	v1=v1(ss) 
		v2=v2(ss)
		end
        3: begin 
		v1=v1(ss)
	   	v2=v2(ss) & v3=v3(ss)
		end
        4: begin 
		v1=v1(ss)
	   	 v2=v2(ss) & v3=v3(ss) & v4=v4(ss)
		end
        5: begin 
		 v1=v1(ss)
	   	 v2=v2(ss) & v3=v3(ss) & v4=v4(ss) & v5=v5(ss)
		end
        6: begin 
		 v1=v1(ss)
	   	 v2=v2(ss) & v3=v3(ss) & v4=v4(ss) & v5=v5(ss) & v6=v6(ss)
		end
        7: begin 
		 v1=v1(ss)
	   	 v2=v2(ss) & v3=v3(ss) & v4=v4(ss) & v5=v5(ss) & v6=v6(ss) & v7=v7(ss)
		end
        8: begin 
		 v1=v1(ss)
	   	 v2=v2(ss) & v3=v3(ss) & v4=v4(ss) & v5=v5(ss) & v6=v6(ss) & v7=v7(ss) & v8=v8(ss)
		end
        9: begin 
		 v1=v1(ss)
	   	 v2=v2(ss) & v3=v3(ss) & v4=v4(ss) & v5=v5(ss) & v6=v6(ss) & v7=v7(ss) & v8=v8(ss) & v9=v9(ss)
		end
	else:
 endcase
return
end