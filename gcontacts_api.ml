open Printf
open Lwt
open Log
open Util_url.Op
open Gcontacts_api_t

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
               ^ "/full" in
    let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json";
                 "GData-Version", "3.0"] in
    let url = Google_api_util.make_uri
      ~host:"www.google.com"
      ~query:["alt",["json"]]
      path
    in
    Http.get ~headers url
  ) >>= function
  | (`OK, _headers, body) -> return body
  | (status,headers,body) -> Google_http.fail "contacts" status body
