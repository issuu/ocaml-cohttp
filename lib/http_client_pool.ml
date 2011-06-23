(*pp camlp4o -I `ocamlfind query lwt.syntax` lwt-syntax-options.cma lwt-syntax.cma *)

(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
  Copyright (C) <2009> David Sheets <sheets@alum.mit.edu>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

module type Http_conf =
  sig 
    val nb_socket: int
    val host: string
    val port: int
 end
 
 
module Make = functor (C : Http_conf) -> 
 struct 
    open Printf
    open Http_common
    open Lwt

    type headers = (string * string) list

    type request_body = [ 
    | `None 
    | `String of string 
    | `InChannel of (int * Lwt_io.input_channel) 
    (* a channel from which to fill the request's body, and its length,
       which must be known in advance *)
    ]

    type tcp_error_source = Connect | Read | Write
    exception Tcp_error of tcp_error_source * exn
    exception Http_error of (int * headers * string)  (* code, headers, body *)

    let display fmt =
      Printf.ksprintf (fun s -> print_endline s; flush stdout) fmt

    exception No_pool of string
    let sock_pool = ref None
      
    let call headers kind request_body url response_body input output =
      let meth = match kind with
        | `GET -> "GET"
        | `HEAD -> "HEAD"
        | `PUT -> "PUT" 
        | `DELETE -> "DELETE" 
        | `POST -> "POST" in
      let endp = Http_client.parse_url url in
      (try_lwt
        display "url = %s\n" url;
         Http_client.request output headers meth request_body endp
       with exn ->
         display "Error in write %s" (Printexc.to_string exn);
         fail (Tcp_error (Write, exn))
      ) >>= fun _ -> (
        try_lwt
          Http_client.read_response input response_body
        with
          | (Http_error _) as e -> display "Error in read %s" (Printexc.to_string e); fail e
          | exn -> display "Error in read 2 %s" (Printexc.to_string exn); fail (Tcp_error (Read, exn))
       )


    (* if a error is raise lwt_pool.use recreate a new pool with the function create_socket *)
    let rec access_pool ?(loop=0) headers kind request_body url response_body =
      let sock_pool = match !sock_pool with
         | Some p -> p
         | None -> raise (No_pool "the pool has not been created, maybe you forget to call: init address port")
       in
      try_lwt
        Lwt_pool.use sock_pool (fun (input, output) ->
         call headers kind request_body url response_body input output)
      with exp ->
        (* if the request fail 5 time in a row, drop the request and raise the exception *)
        if loop >= 5
        then fail exp
        else access_pool ~loop:(loop + 1) headers kind request_body url response_body


    let call_to_string headers kind request_body url =
      lwt resp = access_pool headers kind request_body url `String in
      match resp with
        | `S hb -> return hb
        | _ -> assert false

    let call_to_chan headers kind request_body url outchan =
      lwt resp = access_pool headers kind request_body url (`OutChannel outchan) in
      (* assert relation between request and response kind *)
      match resp with
        | `C h -> return h
        | _ -> assert false

    let head   ?headers               url = call_to_string headers `HEAD   `None url
    let get    ?headers               url = call_to_string headers `GET    `None url
    let post   ?headers ?(body=`None) url = call_to_string headers `POST    body url
    let put    ?headers ?(body=`None) url = call_to_string headers `PUT     body url
    let delete ?headers               url = call_to_string headers `DELETE `None url

    let head_to_chan   ?headers               url ch = call_to_chan headers `HEAD   `None url ch
    let get_to_chan    ?headers               url ch = call_to_chan headers `GET    `None url ch
    let post_to_chan   ?headers ?(body=`None) url ch = call_to_chan headers `POST    body url ch
    let put_to_chan    ?headers ?(body=`None) url ch = call_to_chan headers `PUT     body url ch
    let delete_to_chan ?headers               url ch = call_to_chan headers `DELETE `None url ch


    let memory = ref []


    let create_socket host port =
      display "create socket function";
      let hent = Unix.gethostbyname host in
      let sockaddr = Unix.ADDR_INET (hent.Unix.h_addr_list.(0), port) in
      lwt (input, output) = Lwt_io.open_connection sockaddr in
      memory := (input, output)::!memory;
      
      (* display "end of create function"; *)
      return (input, output)

    let init sock_nb host port =
      sock_pool := Some (Lwt_pool.create sock_nb ~check:(fun _ f -> f false) (fun _ -> create_socket host port))

    let close_all_socket () =
      sock_pool := None;
      Lwt_list.iter_p (
        fun (input, output) -> try_lwt Lwt_io.close input >> Lwt_io.close output with _ -> return ()
      ) !memory >>= fun _ ->
        memory := [];
        return ()


    let _ =
      let (host, _, _) = Http_client.parse_url C.host in
      display "nb_socket = %d, host = %s, port = %d" C.nb_socket C.host C.port;
      init C.nb_socket host C.port

end

