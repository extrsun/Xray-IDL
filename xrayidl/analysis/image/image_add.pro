function image_add,array,array_loc,image,image_loc
      for k=0L,(n_elements(image_loc)-1) do begin
	k_array=array_loc(k)
  	array(k_array) = array(k_array) + image(image_loc(k))
      endfor
return,array
end