(* http pool, for massive parallel connections *)

open Lwt
let display fmt = Printf.ksprintf (fun s -> print_endline s) fmt


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
      let create () = Printf.printf "o" ; flush stdout ; Lwt_io.open_connection sockaddr in 
      let check _ f = f false in
      Lwt_pool.create ~check pool_size create 

     let lookup endpoint = 
      try 
        Hashtbl.find links endpoint 
      with Not_found -> let pool = create_pool endpoint in Hashtbl.add links endpoint pool ; pool

    (* helpers ************************************************************************)
         
     let read ic =
       let buf = Buffer.create 0 in 
       let rec loop ic =
         Lwt_io.read_line ic 
         >>= function
           | "" -> return (Buffer.contents buf)
           | s -> Buffer.add_string buf s; loop ic in 
       loop ic

     let read_chunked inchan = 
       let buf = Buffer.create 0 in 
       let rec loop ic = 
         Lwt_io.read_line ic 
         >>= function
           | "0" ->  Lwt_io.read_line ic >>= fun _ -> return (Buffer.contents buf)
           | size_s -> let count = int_of_string ("0x"^size_s) in 
                       let s = String.create count in 
                       Lwt_io.read_into_exactly inchan s 0 count
                       >>= fun _ -> Buffer.add_string buf s ; Lwt_io.read_line ic >>= fun _ -> loop ic in
       loop inchan 
     
     let check_headers label headers = 
       try 
         Some (List.assoc label headers)
       with Not_found -> None 

     let read_response inchan =
          lwt (_, status) = Http_parser.parse_response_fst_line inchan in
          lwt headers = Http_parser.parse_headers inchan in
          let headers = List.map (fun (h, v) -> (String.lowercase h, v)) headers in
          (* List.iter (fun (h,v) -> print_endline (h  ^ " : " ^ v)) headers ; *)
          (* a status code of 206 (Partial) will typicall accompany "Content-Range" 
             response header *)
          
          match check_headers "transfer-encoding" headers with 
            | Some "chunked" ->
              lwt resp = read_chunked inchan in 
              (match code_of_status status with
                | 200 | 206 -> return (`S (headers, resp))
                | code -> fail (Http_client.Http_error (code, headers, resp)))
            | _ -> 
               lwt resp = read inchan in
               (match code_of_status status with
                 | 200 | 206 -> return (`S (headers, resp))
                 | code -> fail (Http_client.Http_error (code, headers, resp)))
            
                            
     let rec call ?(retry=true) headers kind request_body endp (i, o) =
       let meth = match kind with
         | `GET -> "GET"
         | `HEAD -> "HEAD"
         | `PUT -> "PUT" 
         | `DELETE -> "DELETE" 
         | `POST -> "POST" in
   
           (try_lwt
              Http_client.request ~http:`HTTP_1_1 o headers meth request_body endp
            with exn -> 
              fail (Http_client.Tcp_error (Http_client.Write, exn))
           ) >> (
             try_lwt
               read_response i
                 with
                   | (Http_client.Http_error (503, h, c)) when retry -> 
                     ( 
                       Printf.printf "R" ; flush stdout;
                       Lwt_unix.sleep 0.1 
                       >>= fun _ -> call ~retry:false headers kind request_body endp (i, o))
                   | (Http_client.Http_error (code, h, c)) as e -> fail e
                   | exn -> fail (Http_client.Tcp_error (Http_client.Read, exn)))
        
     let call_to_string headers kind request_body url =
       let (host, port, _) as endp = Http_client.parse_url url in
       let links = lookup (host, port) in 
       Lwt_pool.use links
         (fun s ->
           lwt resp = call headers kind request_body endp s in
           (* assert relation between request and response kind *)
           match resp with
             | `S hb -> return hb
             | _ -> assert false)
        
    (* command ************************************************************************)

     let get ?headers url = call_to_string headers `GET `None url
     let post ?headers ?(body=`None) url = call_to_string headers `POST body url
  end 
