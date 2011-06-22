module type Http_conf =
  sig 
    val nb_socket : int 
    val host : string
    val port : int 
end
module Make :
  functor (C : Http_conf) ->
    sig
      type headers = (string * string) list
      
      type request_body =
          [ `InChannel of int * Lwt_io.input_channel
          | `None
          | `String of string ]
      
      type tcp_error_source = Connect | Read | Write
      
      exception Tcp_error of tcp_error_source * exn
      exception Http_error of (int * headers * string)
      exception No_pool of string
      
      val head :
        ?headers:Http_client.headers ->
        string -> (Http_client.headers * string) Lwt.t
      
      val get :
        ?headers:Http_client.headers ->
        string -> (Http_client.headers * string) Lwt.t
      
      val post :
        ?headers:Http_client.headers ->
        ?body:Http_client.request_body ->
        string -> (Http_client.headers * string) Lwt.t
      
      val put :
        ?headers:Http_client.headers ->
        ?body:Http_client.request_body ->
        string -> (Http_client.headers * string) Lwt.t
      
      val delete :
        ?headers:Http_client.headers ->
        string -> (Http_client.headers * string) Lwt.t
      
      val head_to_chan :
        ?headers:Http_client.headers ->
        string -> Lwt_io.output_channel -> Http_client.headers Lwt.t
      
      val get_to_chan :
        ?headers:Http_client.headers ->
        string -> Lwt_io.output_channel -> Http_client.headers Lwt.t
      
      val post_to_chan :
        ?headers:Http_client.headers ->
        ?body:Http_client.request_body ->
        string -> Lwt_io.output_channel -> Http_client.headers Lwt.t
      
      val put_to_chan :
        ?headers:Http_client.headers ->
        ?body:Http_client.request_body ->
        string -> Lwt_io.output_channel -> Http_client.headers Lwt.t
      
      val delete_to_chan :
        ?headers:Http_client.headers ->
        string -> Lwt_io.output_channel -> Http_client.headers Lwt.t

    end
