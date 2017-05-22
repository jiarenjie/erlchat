%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 五月 2017 9:47
%%%-------------------------------------------------------------------
-module(erlchat).
-author("jiarj").
-include("include/wechat.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([get_code_url/3, get_access_token/3, refresh_token/2, get_userinfo/2, check/4, send_template_message/2]).

get_code_url(Appid,Redirect_uri,State) when is_list(Appid),is_list(Redirect_uri),is_list(State) ->
  Enc_url=edoc_lib:escape_uri(Redirect_uri),
  Url = ?API_URL_CODE++"?appid="++Appid++"&redirect_uri="++Enc_url++"&response_type=code&scope=snsapi_userinfo&state=" ++ State ++"#wechat_redirect",
  Url.
get_access_token(Appid,Secret,Code) when is_list(Appid),is_list(Secret),is_binary(Code) ->
  get_access_token(Appid,Secret,binary_to_list(Code));
get_access_token(Appid,Secret,Code) when is_list(Appid),is_list(Secret),is_list(Code)->
  %Access_token_Rui = "https://api.weixin.qq.com/sns/oauth2/access_token?appid="++Appid++"&secret="++Secret++"&code="++binary_to_list(Code)++"&grant_type=authorization_code",
  Access_token_Rui = ?API_URL_ACCESS_TOKEN++"?appid="++Appid++"&secret="++Secret++"&code="++Code++"&grant_type=authorization_code",
  Response= wechat_util:http_get(Access_token_Rui),
  Response;
get_access_token(app,Appid,Secret) when is_list(Appid),is_list(Secret) ->
  %Access_token_Rui = "https://api.weixin.qq.com/sns/oauth2/access_token?appid="++Appid++"&secret="++Secret++"&code="++binary_to_list(Code)++"&grant_type=authorization_code",
  Access_token_Rui = "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&"++"appid="++Appid++"&secret="++Secret,
  Response= wechat_util:http_get(Access_token_Rui),
  Response.
refresh_token(Appid,REFRESH_TOKEN) when is_list(Appid),is_binary(REFRESH_TOKEN)->
  refresh_token(Appid,binary_to_list(REFRESH_TOKEN));
refresh_token(Appid,REFRESH_TOKEN) when is_list(Appid),is_list(REFRESH_TOKEN) ->
  %Access_token_Rui = "https://api.weixin.qq.com/sns/oauth2/access_token?appid="++Appid++"&secret="++Secret++"&code="++binary_to_list(Code)++"&grant_type=authorization_code",
  REFRESH_TOKEN_Rui = ?API_URL_REFRESH_TOKEN++"?appid="++Appid++"&grant_type=refresh_token&refresh_token="++REFRESH_TOKEN,
  Response= wechat_util:http_get(REFRESH_TOKEN_Rui),
  Response.


get_userinfo(ACCESS_TOKEN,OPENID) when is_binary(ACCESS_TOKEN),is_binary(OPENID)->
  get_userinfo(binary_to_list(ACCESS_TOKEN),binary_to_list(OPENID));
get_userinfo(ACCESS_TOKEN,OPENID) when is_list(ACCESS_TOKEN),is_list(OPENID)->
  %Userinfo_Url = "https://api.weixin.qq.com/sns/userinfo?access_token="++binary_to_list(ACCESS_TOKEN)++"&openid="++binary_to_list(OPENID),
  Userinfo_Url =?API_URL_USERINFO++"?access_token="++ACCESS_TOKEN++"&openid="++OPENID++"&lang=zh_CN",
  Userinfo= wechat_util:http_get(Userinfo_Url),
  Userinfo.

check(Signature, Timestamp, Nonce,Token) when is_binary(Signature),is_binary(Timestamp),is_binary(Nonce),is_binary(Token) ->
  check(binary_to_list(Signature), binary_to_list(Timestamp), binary_to_list(Nonce),binary_to_list(Token));
check(Signature, Timestamp, Nonce,Token) when is_list(Signature),is_list(Timestamp),is_list(Nonce),is_list(Token)->
  TmpList = [Token, Timestamp, Nonce],
  TmpList2 = lists:sort(TmpList),
  TmpStr = string:join(TmpList2, ""),
  Hash = wechat_util:hexstring(TmpStr),
  string:equal(string:to_lower(Signature), string:to_lower(Hash)).

send_template_message(AccessToken, Message) when is_binary(AccessToken),is_binary(Message) ->
  send_template_message(binary_to_list(AccessToken), Message);
send_template_message(AccessToken, Message) when is_list(AccessToken),is_binary(Message) ->
  URL = ?API_URL_PREFIX ++ "/cgi-bin/message/template/send?access_token="++AccessToken,
  wechat_util:http_post(URL, Message).


check_test()->
  Check = check(<<"95a0d735a1b79c3f23054f2dd1068884f654ea4a">>,<<"1492407520">>,<<"1056732191">>,<<"wechat">>),
  ?assertEqual(true,Check),
  ok.

get_code_url_test()->
  Code_url=get_code_url("wxa385560bcf4ffdd1"
    ,"http://127.0.0.1:8081/wechat/callback","123456"),
  Expected = "https://open.weixin.qq.com/connect/oauth2/authorize?appid=wxa385560bcf4ffdd1&redirect_uri=http%3a%2f%2f127.0.0.1%3a8081%2fwechat%2fcallback&response_type=code&scope=snsapi_userinfo&state=123456#wechat_redirect",
  ?assertEqual(Expected,Code_url),
  ok.

