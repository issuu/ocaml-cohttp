open Lwt

let display fmt = Printf.ksprintf (fun s -> print_endline s) fmt

module Client = Http_pool.Make (Config)


let google () = 
  Client.get "http://www.google.fr" 
  >>= function (_, s) -> Printf.printf "." ; flush stdout ; return ()
   

let speedtest f n = 
  let t1 = Unix.gettimeofday () in 
  f n ; 
  let t2 = Unix.gettimeofday () in
  display "runtime: %fs" (t2 -. t1)

let parallel n = 
  Lwt_main.run (
    let googles = Array.to_list (Array.init n (fun i -> google)) in 
    Lwt_list.iter_p (fun f -> f ()) googles)  
  
let sequential n = 
  Lwt_main.run (
    let googles = Array.to_list (Array.init n (fun i -> google)) in 
    Lwt_list.iter_s (fun f -> f ()) googles)


let _ = 
  let n = try int_of_string Sys.argv.(1) with _ -> 100 in 
  display "> cohttp test" ; 
  display "> sequential" ; 
  speedtest sequential n;
  display "> parallel" ; 
  speedtest parallel n ; 
  Client.clean ()
  
