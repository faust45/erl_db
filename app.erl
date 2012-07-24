-module(app).

-export([start/0, listen/1]).
-include_lib("kernel/include/file.hrl").

-record(state, {port, asock, request_count = 0}).
-record(req, {method, path, bin_path, headers}).
-define(_if(Predicate, TrueCase, FalseCase), if Predicate -> TrueCase; true -> FalseCase end).


start() ->
    {ok, LSock} = gen_tcp:listen(8000, [binary, {packet, http}, {reuseaddr, true}, {active, true}]),
    listen(LSock).

listen(LSock) ->
    {ok, AcceptSocket} = gen_tcp:accept(LSock),
    spawn(?MODULE, listen, [LSock]),
    receive_req(#req{}).

receive_req(Req) -> 
    receive
      {http, Socket, {http_request, Method, {abs_path, Path}, Version}} ->
          receive_req(Req#req{method = Method, path = Path, bin_path = list_to_binary(Path),  headers = []});
      {http, Socket, {http_header, _, Name, _, Value}} ->
          Headers = Req#req.headers,
          receive_req(Req#req{headers = [{Name, Value} | Headers]});
      {http, Socket, http_eoh} ->
          Resp = handle_action(Req#req.method, Req#req.bin_path, Req),
          send_headers(Socket, Resp),
          send_responce(Socket, Resp),
          ok = gen_tcp:close(Socket);
      {tcp_closed, Socket} ->
          io:format("Socket ~p closed~n", [Socket]);
      _Any ->
          io:format("Got unexpected message ~p ~n", [_Any])
    end.

handle_action(_, <<"/public/", Path/binary>>, Req) ->
    static_resp(Path);

handle_action('GET', Path, Req) ->
    dinamic_resp(<<"Some">>).

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

resp_headers(Headers) ->
    lists:append([<<"HTTP/1.1 200 OK\r\n">>, <<"Content-Type: text/html\r\n">>], Headers).


