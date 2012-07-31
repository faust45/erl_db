-module(app).

-export([start/0, listen/1]).
-include_lib("kernel/include/file.hrl").

-record(state, {port, asock, request_count = 0}).
-record(req, {method, path, a, bin_path, content_type, content_length, headers}).
-record(form_data, {content_type, param_name, value = <<>>}).
-define(_if(Predicate, TrueCase, FalseCase), if Predicate -> TrueCase; true -> FalseCase end).
-define(_first_def_value(FirstValue, SecondValue), if FirstValue -> FirstValue ; true -> SecondValue end).



start() ->
    {ok, LSock} = gen_tcp:listen(8000, [binary, {packet, http}, {reuseaddr, true}, {active, once}]),
    listen(LSock).

listen(LSock) ->
    {ok, AcceptSocket} = gen_tcp:accept(LSock),
    spawn(?MODULE, listen, [LSock]),
    receive_req(#req{a=AcceptSocket}).

receive_req(Req) -> 
    receive
      {http, Socket, {http_request, Method, {abs_path, Path}, Version}} ->
          inet:setopts(Socket, [{active, once}]),
          receive_req(Req#req{method = Method, path = Path, bin_path = list_to_binary(Path),  headers = []});
      {http, Socket, {http_header, _, Name, _, Value}} ->
          inet:setopts(Socket, [{active, once}]),
          Headers = Req#req.headers,
          receive_req(Req#req{headers = [{Name, Value} | Headers]});
      {http, Socket, http_eoh} ->
          inet:setopts(Socket, [{packet, raw}]),
          Headers = Req#req.headers,
          Req1 = prepare_req(Req),
          Body = consume_body(Socket, Req1#req.content_type, Req1#req.content_length),
          Resp = handle_action(Req#req.method, Req#req.bin_path, Req1),
          send_headers(Socket, Resp),
          send_responce(Socket, Resp),
          ok = gen_tcp:close(Socket);
      {tcp_closed, Socket} ->
          io:format("Socket ~p closed~n", [Socket]);
      _Any ->
          io:format("Got unexpected message ~p ~n", [_Any])
    end.

handle_action('GET', <<"/public/", Path/binary>>, Req) ->
    static_resp(Path);

handle_action('GET', Path, Req) ->
    dinamic_resp(<<"Some">>);

handle_action('POST', Path, Req) ->
    dinamic_resp(<<"'Some'">>).

dinamic_resp(Data) ->
    Length = byte_size(Data),
    {dinamic, Length, Data}.

static_resp(Path) ->
    Path1 = <<"www/", Path/binary>>,
    case file:read_file_info(Path1) of
        {ok, FileInfo} ->
            {static, FileInfo#file_info.size, Path1};
        {error, Reason} ->
            io:format("got error when try read file info ~p ~n", [Reason])
   end.
    
send_responce(Socket, {static, _, Path}) ->
    file:sendfile(Path, Socket);

send_responce(Socket, {dinamic, _, Data}) ->
    gen_tcp:send(Socket, Data).

send_headers(Socket, {_, Length, _}) ->
   Headers = resp_headers([<<"Content-Length: " >>, integer_to_list(Length), "\r\n"]),
   gen_tcp:send(Socket, Headers),
   gen_tcp:send(Socket, "\r\n").

consume_body(Socket, <<"multipart/form-data; boundary=", Boundary/binary>>, Length) ->
    inet:setopts(Socket, [{packet, line}]),
    Data = do_recv(Socket, <<"--", Boundary/binary>>, Length, 0, []),
    io:format("Debug in multipart ~p ~n", [Data]),
    ok;

consume_body(Socket, Type, Length) ->
    ok.

resp_headers(Headers) ->
    lists:append([<<"HTTP/1.1 200 OK\r\n">>, <<"Content-Type: text/html\r\n">>], Headers).

prepare_req(Req) ->
    Req1 = Req#req{content_type    = s_try(fun list_to_binary/1, header('Content-Type', Req))},
    Req2 = Req1#req{content_length = s_try(fun list_to_integer/1, header('Content-Length', Req))},
    Req2.

fetch_to_buffer(Socket, Boundary, MaxLength, []) ->
    fetch_to_buffer(Socket, Boundary, MaxLength, [#form_data{}]);

fetch_to_buffer(Socket, Boundary, MaxLength, List = [Item|Buff]) when 0 < MaxLength ->
    ParamEndBoundary   = <<Boundary/binary, "\r\n">>,
    ContentEndBoundary = <<Boundary/binary, "--\r\n">>,

    case gen_tcp:recv(Socket, 0, 100) of
      {ok, Line} ->
         NewBuff =
         case Line of
           ParamEndBoundary ->
               [#form_data{} | List];
           ContentEndBoundary ->
               List;
           <<"Content-Disposition: form-data; name=", Name/binary>> ->
               V = Item#form_data{param_name = Name},
               [V | Buff];
           <<"Content-Type: ", Type/binary>> ->
               V = Item#form_data{content_type = Type},
               [V | Buff];
           <<"\r\n">> ->
               List;
           _ ->
               Value = Item#form_data.value,
               V = Item#form_data{value = <<Value/binary, Line/binary>>},
               [V | Buff]
           end,

           fetch_to_buffer(Socket, Boundary, MaxLength - byte_size(Line), NewBuff);
      {error, timeout} ->
          io:format("Got Timeout ~n", []),
          List
    end;

fetch_to_buffer(_, _, _, Buff) ->
    Buff.

do_recv(Socket, Boundary, Length, RecvLength, Data) ->
    Buff = fetch_to_buffer(Socket, Boundary, Length, []),
    io:format("Debug in size fetch buff ~p ~n", [length(Buff)]),
    Buff.

s_try(Fun, Value) ->
    case Value of
      undefined -> undefined;
      _ -> try Fun(Value) of
              V -> V
           catch
              _ -> ok
           end
    end.

header(Name, Req = #req{headers = Headers}) ->
    proplists:get_value(Name, Headers).


