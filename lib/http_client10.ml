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

open Printf
open Http_common
open Lwt

type headers = (string * string) list

type tcp_error_source = Connect | Read | Write
exception Tcp_error of tcp_error_source * exn
exception Http_error of (int * headers * string)  (* code, body *)

let http_scheme_RE = Pcre.regexp ~flags:[`CASELESS] "^http://"
let url_RE = Pcre.regexp "^([\\w.-]+)(:(\\d+))?(/.*)?$"

let tcp_bufsiz = 4096 (* for TCP I/O *)

let display fmt =
  Printf.ksprintf (fun s -> print_endline s; flush stdout) fmt

let parse_url url =
  try
    let subs =
      Pcre.extract ~rex:url_RE (Pcre.replace ~rex:http_scheme_RE url)
    in
    (subs.(1),
    (if subs.(2) = "" then 80 else int_of_string subs.(3)),
    (if subs.(4) = "" then "/" else subs.(4)))
  with exc ->
    failwith
      (sprintf "Can't parse url: %s (exception: %s)"
        url (Printexc.to_string exc))

let rec read_write_r inchan outchan read_size num_read max_to_read =
  lwt s = Lwt_io.read ~count:read_size inchan in
  if s = ""  then
    return ()
  else
    lwt () = Lwt_io.write outchan s in
    let num_read_incr = num_read + (String.length s) in
    if num_read_incr < max_to_read then
      read_write_r inchan outchan read_size num_read_incr max_to_read
    else
      return ()

let read_write ?(read_size=tcp_bufsiz) inchan outchan =
  read_write_r inchan outchan read_size 0 max_int

let read_write_count ?(read_size=tcp_bufsiz) inchan outchan ~count =
  read_write_r inchan outchan read_size 0 count
  

(* the source of a request body, if any, can be either a string or an
   input channel *)
type request_body = [ 
| `None 
| `String of string 
| `InChannel of (int * Lwt_io.input_channel) 
]

let content_length_header s = 
  "Content-Length", s 

let default_content_type_h = "Content-Type", "application/x-www-form-urlencoded"

let build_req_header ?(http=`HTTP_1_0) headers meth address path body =
  let headers = 
    match body with
      | `None -> headers
      | `String s -> 
        let content_length_h = content_length_header (string_of_int (String.length s)) in
        default_content_type_h :: content_length_h :: headers
      | `InChannel (cl,_) -> 
        let content_length_h = content_length_header (string_of_int cl) in
        default_content_type_h :: content_length_h ::headers
  in
  let headers = ("Host", address) :: headers in
  let hdrcnt = List.length headers in
  let add_header ht (n, v) = (Hashtbl.replace ht n v; ht) in
  let hdrht = List.fold_left add_header (Hashtbl.create hdrcnt) headers in
  let serialize_header name value prev =
    sprintf "%s\r\n%s: %s" prev name value in
  let hdrst = Hashtbl.fold serialize_header hdrht "" in
  sprintf "%s %s HTTP/1.%d%s\r\n\r\n" meth path (match http with `HTTP_1_1 -> 1 | `HTTP_1_0 -> 0) hdrst

let request ?(http=`HTTP_1_0) outchan headers meth body (address, _, path) =
  let headers = match headers with None -> [] | Some hs -> hs in
  let req_header = build_req_header ~http headers meth address path body in
  
  lwt () = Lwt_io.write outchan req_header in
  lwt () =
    match body with
      | `None -> return ()
      | `String s ->
        Lwt_io.write outchan s
      | `InChannel (content_length, inchan) ->
        read_write inchan outchan 
  in
  Lwt_io.flush outchan


let id x = x

let parse_content_range headers =
  try
    let s = List.assoc "content-range" headers in
    let start, fini, total = Scanf.sscanf s "bytes %d-%d/%d" 
      (fun start fini total -> start, fini, total) 
    in
    Some (start, fini, total)
  with
    | Not_found | Scanf.Scan_failure _ ->
        None


(* if we see a "Content-Range" or "Content-length" header, than we should limit the
   number of bytes we attempt to read *)
let content_length_of_content_range headers =
  try
    (* assuming header keys were downcased in previous step *)
    let range_s = parse_content_range headers in (* check http protocole to see if content_lenght is before of after content-range *)
    let content_lenght = try Some (List.assoc "content-length" headers) with Not_found -> None in
    match range_s, content_lenght with
      | Some (start, fini, total), _ ->
          (* some sanity checking before we act on these values *)
        if fini < total && start <= total && 0 <= start && 0 <= total then (
          let num_bytes_to_read = fini - start + 1 in
          Some num_bytes_to_read
        )
        else
          None
      | None, Some nb ->
          Some (int_of_string nb)
      | None, None ->
        None
  with Not_found ->
    None


let rec read_exactly inchan acc = 
  function 
    | 0 -> return acc 
    | count -> Lwt_io.read ~count inchan >>= fun s -> read_exactly inchan (acc ^ s) (count - (String.length s))

let read_response ?(head=false) inchan response_body =
  lwt (_, status) = Http_parser.parse_response_fst_line inchan in
  lwt headers = Http_parser.parse_headers inchan in
  (* List.iter (fun (el,el2) -> display "header %s %s\n" el el2) headers; *)
  let headers = List.map (fun (h, v) -> (String.lowercase h, v)) headers in
  let content_length_opt = content_length_of_content_range headers in
  (* a status code of 206 (Partial) will typicall accompany "Content-Range" 
     response header *)
  match response_body with
    | `String -> (
      lwt resp = 
        match head, content_length_opt with
          | false, Some count -> read_exactly inchan "" count 
          | _ -> Lwt_io.read inchan
      in
      match code_of_status status with
        | 200 | 206 -> return (`S (headers, resp))
        | code -> fail (Http_error (code, headers, resp))
      )
    | `OutChannel outchan -> (
      lwt () = 
        match head, content_length_opt with
          | false, Some count -> read_write_count ~count inchan outchan 
          | _ -> read_write inchan outchan
      in
      match code_of_status status with
        | 200 | 206 -> return (`C headers)
        | code -> fail (Http_error (code, headers, ""))
      )

let connect (address, port, _) iofn =
  lwt sockaddr = Http_misc.build_sockaddr (address, port) in
  Lwt_io.with_connection sockaddr iofn
  
let call headers kind request_body url response_body =
  let meth = match kind with
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `PUT -> "PUT" 
    | `DELETE -> "DELETE" 
    | `POST -> "POST" in
  let endp = parse_url url in
  try_lwt connect endp
    (fun (i, o) ->
      (try_lwt
         request o headers meth request_body endp
       with exn -> 
         fail (Tcp_error (Write, exn))
      ) >> (
        try_lwt
          read_response ~head:(kind = `HEAD) i response_body
        with
          | (Http_error _) as e -> fail e
          | exn -> fail (Tcp_error (Read, exn))
       ))
  with
    | (Tcp_error _ | Http_error _) as e -> fail e
    | exn -> fail (Tcp_error (Connect, exn))

let call_to_string headers kind request_body url =
  lwt resp = call headers kind request_body url `String in
  (* assert relation between request and response kind *)
  match resp with
    | `S hb -> return hb
    | _ -> assert false

let call_to_chan headers kind request_body url outchan =
  print_endline "legacy http client called" ;
  lwt resp = call headers kind request_body url (`OutChannel outchan) in
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
