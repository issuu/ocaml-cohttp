open Lwt

(* misc utilities *******************************************************************)

let display fmt = Printf.ksprintf (fun s -> print_endline s) fmt
let mesure f a _ = 
  let open Unix in 
      let t1 = times () in 
      let t1' = gettimeofday () in 
      f a >>= fun _ -> let t2 = times () in
                        let t2' = gettimeofday () in 
                        return (t2' -. t1',
                                t2.tms_utime -. t1.tms_utime, 
                                t2.tms_stime -. t1.tms_stime)

(* http machinery *******************************************************************)

module Client = 
  struct 
    include Http_client
    include Http_pool.Make (Config)
  end

module Sdb = SDB_factory.Make (Client)

(* command **************************************************************************)

let touch item_name () = 
  Sdb.put_attributes ~replace:true
    Keys.creds
    (Config.get_param "domain")
    item_name
    [ "last_access", string_of_float (Unix.time ()) ]
    >>= fun _ -> return ()

let create_domain _ = 
  Sdb.create_domain Keys.creds (Config.get_param "domain")
    
let delete_domain _ = 
  Sdb.delete_domain Keys.creds (Config.get_param "domain")

let rec sequential =
  function 
    | 0 -> return () 
    | n ->
      catch 
        (fun () -> touch (Printf.sprintf "item_%d" (Random.int 10000)) () >>= fun _ -> Printf.printf "." ; flush stdout ; return ())
        (fun e -> Printf.printf "X" ; flush stdout; return ())
      >>= fun () -> sequential (n-1)

let i = ref 0 
  
let parallel n = 
  let l = Array.to_list ( Array.init n (fun _ -> touch (Printf.sprintf "item_%d" (Random.int 10000))) ) in 
  let l = List.map
    (fun f -> catch 
      (fun () -> f () >>= fun _ -> Printf.printf "."; flush stdout; return ())
      (fun _ -> Printf.printf "X" ; flush stdout ; return ())
    ) l in
  Lwt.join l 

(* google *)
(*

let google () = 
  Client.get "http://news.google.com" 
  >>= function (_, s) -> return ()

let parallel n = 
  let l = Array.to_list ( Array.init n (fun _ -> google)) in
  let l = List.map
    (fun f -> catch 
      (fun () -> f () >>= fun _ -> Printf.printf "."; flush stdout; return ())
      (fun _ -> Printf.printf "X" ; flush stdout ; return ())
    ) l in
  Lwt.join l 

*)

    
(* main invocation ******************************************************************)
    
let _ = 
  display "> turbo aws" ; 
  let nb = int_of_string (Config.get_param "nb_request") in
  Lwt_main.run (
    delete_domain () 
    >>= delete_domain
    >>= mesure parallel nb
    >>= fun (t, _, _) -> display "elapsed time: %fs; rps: %f" t ((float_of_int nb) /. t); delete_domain ())
    
