open Lwt

(* misc utilities *******************************************************************)

let display fmt = Printf.ksprintf (fun s -> print_endline s) fmt
let mesure f = 
  let open Unix in 
      let t1 = times () in 
      let t1' = gettimeofday () in 
      f () >>= fun _ -> let t2 = times () in
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

let touch item_name = 
  Sdb.put_attributes ~replace:true
    Keys.creds
    (Config.get_param "domain")
    item_name
    [ "last_access", string_of_float (Unix.time ()) ]
    >>= fun _ -> return ()

(* main invocation ******************************************************************)

let _ = 
  display "> turbo aws" 

    
