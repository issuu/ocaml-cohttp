let params = Hashtbl.create 0 

let get_param = Hashtbl.find params
let set_param = Hashtbl.replace params

let _ = 
  set_param "http_pool_size" "5"
