let params = Hashtbl.create 0 

let get_param = Hashtbl.find params
let set_param = Hashtbl.replace params

let _ = 
  set_param "http_pool_size" "20";
  set_param "domain" "testbesport" 

let _ = 
  Arg.parse 
    [
      "--pool-size", Arg.String (set_param "http_pool_size"), "pool size";
      "--nb-request", Arg.String (set_param "nb_request"), "nb of requests";
    ] print_endline "turboaws : usage is:\n" 
