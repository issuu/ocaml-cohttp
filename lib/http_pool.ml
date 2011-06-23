(* http pool, for massive parallel connections *)

open Lwt

(* remember to add timeouts at some point *)

module type C = 
  sig
    val get_param : string -> string 
  end

module Make (Config: C) = 
  struct

    (* global conf parameters *********************************************************)
    let pool_size = int_of_string (Config.get_param "http_pool_size")
      
    (* pools of alive sockets *********************************************************) 
    let links = Hashtbl.create 0 
    let create_pool (host, port) =
      let hent = Unix.gethostbyname host in
      let sockaddr = Unix.ADDR_INET (hent.Unix.h_addr_list.(0), port) in
      let create () = Lwt_io.open_connection sockaddr in 
      let check _ f = f false in
      Lwt_pool.create ~check pool_size create 

     let lookup endpoint = 
      try 
        Hashtbl.find links endpoint 
      with Not_found -> let pool = create_pool endpoint in Hashtbl.add links endpoint pool ; pool


    (* helpers ************************************************************************)
    
     let call headers kind request_body url response_body =
       let meth = match kind with
         | `GET -> "GET"
         | `HEAD -> "HEAD"
         | `PUT -> "PUT" 
         | `DELETE -> "DELETE" 
         | `POST -> "POST" in
       let (host, port, _) as endp = Http_client.parse_url url in
       let links = lookup (host, port) in 
       Lwt_pool.use links
         (fun (i, o) ->
           (try_lwt
              Http_client.request o headers meth request_body endp
            with exn -> 
              fail (Http_client.Tcp_error (Http_client.Write, exn))
           ) >> (
             try_lwt
               Http_client.read_response i response_body
                 with
                   | (Http_client.Http_error _) as e -> fail e
                   | exn -> fail (Http_client.Tcp_error (Http_client.Read, exn))
            ))
        
     let call_to_string headers kind request_body url =
        lwt resp = call headers kind request_body url `String in
     (* assert relation between request and response kind *)
        match resp with
          | `S hb -> return hb
          | _ -> assert false
        
    (* command ************************************************************************)

     let get ?headers url = call_to_string headers `GET `None url
        
  end 
