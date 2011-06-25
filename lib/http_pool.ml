(* http pool, for massive parallel connections *)

open Lwt

(* remember to add timeouts at some point *)

module type C = 
  sig
    val get_param : string -> string 
  end

module Make (Config: C) = 
  struct
    open Http_common

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

        
     let clean_pool links = 
       
     let clean () = 
       Hashtbl.iter clean_pool links
    (* helpers ************************************************************************)

     let rec read acc ic = 
       Lwt_io.read_line ic 
       >>= function
         | "" -> return acc
         | s -> read (acc^s) ic  

     let read_response inchan response_body =
     lwt (_, status) = Http_parser.parse_response_fst_line inchan in
     lwt headers = Http_parser.parse_headers inchan in
     (* List.iter (fun (el,el2) -> display "header %s %s\n" el el2) headers; *)
     let headers = List.map (fun (h, v) -> (String.lowercase h, v)) headers in
     let content_length_opt = Http_client.content_length_of_content_range headers in
  (* a status code of 206 (Partial) will typicall accompany "Content-Range" 
     response header *)
  
     match response_body with
       | `String -> (
                    lwt resp = 
  
                      match content_length_opt with
                        | Some count -> read "" inchan
                        | None -> read "" inchan
                      in
                    match code_of_status status with
                      | 200 | 206 -> return (`S (headers, resp))
                      | code -> fail (Http_client.Http_error (code, headers, resp))
       )
       | `OutChannel outchan -> (
                                lwt () = 
                                  match content_length_opt with
                                    | Some count -> Http_client.read_write_count ~count inchan outchan 
                                    | None -> Http_client.read_write inchan outchan
                                  in
                                match code_of_status status with
                                  | 200 | 206 -> return (`C headers)
                                  | code -> fail (Http_client.Http_error (code, headers, ""))
       )

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
               read_response i response_body
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
