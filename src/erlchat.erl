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

-behaviour(gen_server).
-record(state, {}).
-define(SERVER, ?MODULE).

%% API
-export([get_code_url/1
  , get_access_token/0
  , get_access_token/1
  , refresh_token/1
  , get_userinfo/1
  , verify/4
  , send_template_message/3 ]).

-export([init/1
  , handle_call/3
  , handle_cast/2
  , handle_info/2
  , terminate/2
  , code_change/3]).

-export([start_link/0]).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}, 0}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  Access_token = get_access_token(),
  io:format("Access_token:~p~n",[Access_token]),
  ok = application:set_env(erlchat, access_token, Access_token),
  {noreply, State, 7000000}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_code_url(Rerirect_url) when is_binary(Rerirect_url)->
  get_code_url(binary_to_list(Rerirect_url));
get_code_url(Rerirect_url) when is_list(Rerirect_url)->
  Appid = get_apppid(),
  Enc_url=edoc_lib:escape_uri(Rerirect_url),
  <<A:32,_B:32,_C:32>> = crypto:strong_rand_bytes(12),
  State = integer_to_list(A),
  Url = ?API_URL_CODE ++ "?appid=" ++ Appid ++
    "&redirect_uri=" ++ Enc_url ++ "&response_type=code&scope=snsapi_userinfo&state=" ++
    State ++ "#wechat_redirect",
  Url.
get_access_token()->
  Appid = get_apppid(),
  Secret=get_secret(),
  Access_token_Rui = "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&"
    ++"appid="++Appid++"&secret="++Secret,
  Response= wechat_util:http_get(Access_token_Rui),
  ResponseJson = jsx:decode(list_to_binary(Response)),
  Access_token=proplists:get_value(<<"access_token">>,ResponseJson),
  Access_token.
get_access_token(Code) when is_binary(Code)->
  get_access_token(binary_to_list(Code));
get_access_token(Code) when is_list(Code)->
  Appid = get_apppid(),
  Secret=get_secret(),
  Access_token_Rui = ?API_URL_ACCESS_TOKEN++"?appid="++Appid++"&secret="++Secret++
    "&code="++Code++"&grant_type=authorization_code",
  Response= wechat_util:http_get(Access_token_Rui),
  Response_Json = jsx:decode(list_to_binary(Response)),
  Response_Json.
refresh_token(Reftesh_token) when is_binary(Reftesh_token)->
  Appid = get_apppid(),
  refresh_token(binary_to_list(Appid),binary_to_list(Reftesh_token)).
refresh_token(Appid,Reftesh_token) when is_list(Appid),is_list(Reftesh_token) ->
  REFRESH_TOKEN_Rui = ?API_URL_REFRESH_TOKEN++"?appid="++Appid++
    "&grant_type=refresh_token&refresh_token="++Reftesh_token,
  Response= wechat_util:http_get(REFRESH_TOKEN_Rui),
  Response_Json = jsx:decode(list_to_binary(Response)),
  Response_Json.
get_userinfo(Openid) when is_binary(Openid)->
  Access_token = env_access_token(),
  get_userinfo(binary_to_list(Access_token),binary_to_list(Openid)).
get_userinfo(Access_token,Openid) when is_list(Access_token),is_list(Openid)->
  Userinfo_Url =?API_URL_USERINFO++"?access_token="++Access_token++"&openid="++Openid++"&lang=zh_CN",
  Userinfo= wechat_util:http_get(Userinfo_Url),
  Response_Json = jsx:decode(list_to_binary(Userinfo)),
  Response_Json.
verify(Signature, Timestamp, Nonce,Token) when
  is_binary(Signature),is_binary(Timestamp),
  is_binary(Nonce),is_binary(Token) ->
    verify(binary_to_list(Signature), binary_to_list(Timestamp), binary_to_list(Nonce),binary_to_list(Token));
verify(Signature, Timestamp, Nonce,Token) when is_list(Signature),is_list(Timestamp),is_list(Nonce),is_list(Token)->
  TmpList = [Token, Timestamp, Nonce],
  TmpList2 = lists:sort(TmpList),
  TmpStr = string:join(TmpList2, ""),
  Hash = wechat_util:hexstring(TmpStr),
  string:equal(string:to_lower(Signature), string:to_lower(Hash)).
send_template_message(Template_id, Openid,Date) when is_list(Template_id),is_binary(Openid),is_map(Date)->
  send_template_message(list_to_binary(Template_id), Openid,Date) ;
send_template_message(Template_id, Openid,Date) when is_binary(Template_id),is_binary(Openid),is_map(Date) ->
  Access_token = env_access_token(),
  URL = ?API_URL_PREFIX ++ "/cgi-bin/message/template/send?access_token="++Access_token,
  Message = process_message(Template_id,Openid,Date),
  Msg2 = jsx:encode(Message),
  wechat_util:http_post(URL, Msg2).


check_test()->
  Check = verify(<<"95a0d735a1b79c3f23054f2dd1068884f654ea4a">>,<<"1492407520">>,<<"1056732191">>,<<"wechat">>),
  ?assertEqual(true,Check),
  ok.

get_apppid() ->
  {ok,Appid}=application:get_env(erlchat,appid),
  Appid.


get_secret() ->
  {ok,Secret} = application:get_env(erlchat,secret),
  Secret.

env_access_token() ->
  {ok,Access_token} = application:get_env(erlchat,access_token),
  Access_token.



process_message(Template_id, Openid, Date) ->
  Date1 = maps:to_list(Date),
  L = [ pr_msg(Msg) || Msg <-Date1 ],
  M =#{<<"touser">> => Openid
    ,<<"template_id">> => Template_id
    ,<<"data">> => L
  },
  maps:to_list(M).

process_message_test() ->
  Template_id = <<"wUKW58uIfGCDNzcf3UIrL1DDSapV6HwAPCW0c6ucVpY">>,
  Openid = <<"ofD9N0wpZmyZrpT5A4FobW055TKY">>,
  Date = #{<<"first">> => <<"first">>
    ,<<"keyword1">> => <<"keyword1">>
    ,<<"keyword2">> => <<"keyword2">>
    ,<<"keyword3">> => <<"keyword3">>
    ,<<"remark">> => <<"remark">>
  },
  Msg = process_message(Template_id,Openid,Date),
  Export = [
    {<<"data">>,[
      {<<"first">>,[{<<"value">>,<<"first">>},{<<"color">>,<<"#173177">>}]}
      ,{<<"keyword1">>,[{<<"value">>,<<"keyword1">>},{<<"color">>,<<"#173177">>}]}
      ,{<<"keyword2">>,[{<<"value">>,<<"keyword2">>},{<<"color">>,<<"#173177">>}]}
      ,{<<"keyword3">>,[{<<"value">>,<<"keyword3">>},{<<"color">>,<<"#173177">>}]}
      ,{<<"remark">>,[{<<"value">>,<<"remark">>},{<<"color">>,<<"#173177">>}]}
    ]}
    ,{<<"template_id">>,<<"wUKW58uIfGCDNzcf3UIrL1DDSapV6HwAPCW0c6ucVpY">>}
    ,{<<"touser">>,<<"ofD9N0wpZmyZrpT5A4FobW055TKY">>}
  ],
  ?assertEqual(Export, Msg).

pr_msg({Key,Val}) ->
  {Key,[{<<"value">>,Val},{<<"color">>,<<"#173177">>}]}.