open Printf
open Lwt
open Log
open Util_url.Op

module Http = Util_http_client.Wrap (Util_http_client.Original) (struct
  type orig_result = Util_http_client.response
  type result = Util_http_client.response
  let wrap f = Cloudwatch.time "google.api.contacts.any" f
end)

type http_response = Util_http_client.response
type with_token = (string -> http_response Lwt.t) -> http_response Lwt.t

let get_contacts with_token email =
  with_token (fun access_token ->
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
  ) >>= function
  | (`OK, _headers, body) ->
      return (Gcontacts_api_j.gcontacts_resp_of_string body)
  | (status,headers,body) -> Google_http.fail "contacts" status body

let get_contacts_next_page with_token string_url =
  with_token (fun access_token ->
    let url = Uri.of_string string_url in
    let headers = [Google_auth.auth_header access_token;
                   "GData-Version", "3.0"] in
    Http.get ~headers url
  ) >>= function
  | (`OK, _headers, body) ->
      return (Gcontacts_api_j.gcontacts_resp_of_string body)
  | (status,headers,body) -> Google_http.fail "contacts" status body

let get_photo with_token email contact_id =
  with_token (fun access_token ->
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
  ) >>= function
  | (`OK, _headers, body) -> return body
  | (status,headers,body) ->
      Google_http.fail "contact photo" status body

let get_photo_with_url with_token string_url =
  with_token (fun access_token ->
    let url = Uri.of_string string_url in
    let headers = [Google_auth.auth_header access_token;
                   "GData-Version", "3.0"] in
    Http.get ~headers url
  ) >>= function
  | (`OK, _headers, body) -> return body
  | (status,headers,body) ->
      Google_http.fail "contact photo" status body
