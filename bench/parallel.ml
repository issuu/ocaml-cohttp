open Lwt

let display fmt = Printf.ksprintf (fun s -> print_endline s) fmt

module Client = Http_pool.Make (Config)


let google () = 
  catch 
    (fun () -> 
      Client.get "http://www.google.fr" 
      >>= function (_, s) -> (* display " >> SIZE %d\n%s\n" (String.length s) s; *) Printf.printf "."; flush stdout;  return ())
    (function 
      | Http_client.Tcp_error (Http_client.Write, e) -> display "tcp error, write, %s" (Printexc.to_string e); fail e
      | Http_client.Tcp_error (Http_client.Read, e) -> display "tcp error, read, %s" (Printexc.to_string e); fail e
      | e -> fail e) 

let speedtest f n = 
  let t1 = Unix.gettimeofday () in 
  f n ; 
  let t2 = Unix.gettimeofday () in
  display "runtime: %fs; %f rps" (t2 -. t1) (float_of_int n /. (t2 -. t1))

let parallel n = 
  Lwt_main.run (
    let googles = Array.to_list (Array.init n (fun i -> google)) in 
    Lwt_list.iter_p (fun f -> f ()) googles)  
  
let sequential n = 
  Lwt_main.run (
    let googles = Array.to_list (Array.init n (fun i -> google)) in 
    Lwt_list.iter_s (fun f -> f ()) googles)

let _ = 
  let n = try int_of_string Sys.argv.(2) with _ -> 100 in 
  display "> cohttp test" ; 
  display "> sequential" ; 
  speedtest sequential n;
  display "> parallel" ; 
   speedtest parallel n  
  
