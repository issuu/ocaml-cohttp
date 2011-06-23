open Lwt

let display fmt = Printf.ksprintf (fun s -> print_endline s) fmt

module Client = Http_pool.Make (Config)


let google () = 
  Client.get "http://www.google.fr" 
  >>= function (_, s) -> display "success" ; return ()
   

let _ = 
  display "> cohttp test" ; 
  Lwt_main.run (
    let googles = Array.to_list (Array.init 6 (fun i -> google ())) in 
    Lwt.join googles)
  
    
  
  
