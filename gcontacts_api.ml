open Printf
open Lwt
open Log
open Util_url.Op

(* Google Contacts API provides direct urls to
   resources which is why 'with_url' functions exist *)

module Http = Util_http_client.Wrap (Util_http_client.Original) (struct
  type orig_result = Util_http_client.response
  type result = Util_http_client.response
  let wrap f = Cloudwatch.time "google.api.contacts.any" f
end)

type http_response = Util_http_client.response
type with_token = (string -> http_response Lwt.t) -> http_response Lwt.t

let call_get_contacts email access_token =
  let s_email = Email.to_string email in
  let path = "m8/feeds/contacts/" ^ Util_url.encode s_email
             ^ "/thin" in
  let headers = [Google_auth.auth_header access_token;
                 "GData-Version", "3.0"] in
  let url = Google_api_util.make_uri
    ~host:"www.google.com"
    ~query:["alt",["json"]]
    path
  in
  Http.get ~headers url

let get_contacts email with_token =
  Cloudwatch.time "google.api.contacts.get_contacts" (fun () ->
    with_token (fun token ->
      call_get_contacts email token
    ) >>= function
    | (`OK, _headers, body) ->
        return (Gcontacts_api_j.gcontacts_resp_of_string body)
    | (status,headers,body) -> Google_http.fail "contacts" status body
  )

let call_with_url string_url access_token =
  let url = Uri.of_string string_url in
  let headers = [Google_auth.auth_header access_token;
                 "GData-Version", "3.0"] in
  Http.get ~headers url

let get_contacts_page string_url with_token =
  Cloudwatch.time "google.api.contacts.get_contacts_page" (fun () ->
    with_token (fun token ->
      call_with_url string_url token
    ) >>= function
    | (`OK, _headers, body) ->
        return (Gcontacts_api_j.gcontacts_resp_of_string body)
    | (status,headers,body) -> Google_http.fail "contacts" status body
  )

let call_get_photo email contact_id access_token =
  let s_email = Email.to_string email in
  let path = "m8/feeds/photos/media/" ^ Util_url.encode s_email
             ^ Util_url.encode contact_id in
  let headers = [Google_auth.auth_header access_token;
                 "GData-Version", "3.0"] in
  let url = Google_api_util.make_uri
    ~host:"www.google.com"
    path
  in
  Http.get ~headers url

let get_photo email contact_id with_token =
  Cloudwatch.time "google.api.contacts.get_photo" (fun () ->
    with_token (fun token ->
      call_get_photo email contact_id token
    ) >>= function
    | (`OK, _headers, body) -> return body
    | (`Not_found, _, _) -> return ""
    | (status,headers,body) ->
        Google_http.fail "contact photo" status body
  )

let get_photo_with_url string_url with_token =
  Cloudwatch.time "google.api.contacts.get_photo_with_url" (fun ()->
    with_token (fun token ->
      call_with_url string_url token
    ) >>= function
    | (`OK, _headers, body) -> return body
    | (`Not_found, _, _) -> return ""
    | (status,headers,body) ->
        Google_http.fail "contact photo" status body
  )
