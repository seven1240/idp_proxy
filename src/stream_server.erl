-module(stream_server).

-behaviour(gen_server).

-include("idp_proxy.hrl"). 

-export([start/1, tt/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gen_server     

start(_Args) ->                              
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) -> 
	io:format("init dffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff~p~n", [_Args]),
	ets:new(stream_table, [named_table,public,set]),
	{ok, {"Start"}}.

handle_call({tt1}, _From, State) ->
	io:format("call tt1"),
	tt1();
	
handle_call(X, _From, State) ->
	io:format("handle_call ~p~n", [X]),
	{reply, ok, State}.
  

handle_cast({download, Bucket, Key, Req}, State) ->
	io:format("downlllllllllllload~n"),
	download(Bucket, Key, Req),
	{noreply, Req};
	
handle_cast(X, State) ->
	io:format("handle_cast ~p~n", [X]),
	{noreply, ok}.

handle_info({ibrowse_async_headers, ReqId, Code, Headers}, _State) ->
	% io:format("async_header~p~n", [Headers]),       
	case ets:lookup(stream_table, ReqId) of
		[{_, {File, Req}}] ->
			ets:delete(stream_table, ReqId),
	
			ContentType = case proplists:get_value("Content-Type", Headers) of
				[] -> "application/octet-stream";
				CT -> CT
			end,
			ContentLength = proplists:get_value("Content-Length", Headers),
			Res = Req:ok({ContentType, [{"Content-Length", ContentLength}], chunked}),                
			ets:insert(stream_table, {ReqId, {File, Res}});
		_ -> noop
	end,
	{noreply, Code};
	
handle_info({ibrowse_async_response, ReqId, Body}, _State) ->
	% io:format("body: ~p~n", ["A"]),         
	case ets:lookup(stream_table, ReqId) of
		[{ReqId, {File, Res}}] ->
			
			% io:format("body: ~p~n", [ReqId]),         
	
	
			file:write(File, Body),
			Res:write_chunk(Body),
			ibrowse:stream_next(ReqId);
		_ -> noop
	end,
	{noreply, ReqId};

handle_info({ibrowse_async_response_end, ReqId}, _State) ->
	io:format("end: ~p~n", [_State]),
	case ets:lookup(stream_table, ReqId) of
		[{ReqId, {File, Res}}] ->
			file:close(File),
			ets:delete(stream_table, ReqId),

			Res:write_chunk(""), 
	
			Req = Res:get(request),
			"/" ++ RPath = Req:get(path),
			LocalPath = filename:absname_join(?DOC_ROOT, RPath),
			case filelib:is_dir(LocalPath) of
				false -> os:cmd("mkdir -p " ++ filename:dirname(LocalPath));
				true -> ok
			end,                                                             
	
			io:format("Move file to the right place: ~p~n", ["mv " ++ " " ++ LocalPath]);
			% os:cmd("mv " ++ TmpFile ++ " " ++ LocalPath),                              
		_ -> noop
	end,
		
	{noreply, ReqId};

		
handle_info(X, _State) ->
	io:format("handle_info ~p~n", [X]),
	{noreply, ok}.                              
	
terminate(normal, State) ->
	io:format("terminate normal~p~n", [State]), 
	ok;

terminate(_Undef, State) ->
	ets:delete(stream_table),
	io:format("terminate undef~p~n", [State]), 
	
	ok.
	
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
		
tt() ->
	io:format("tt~n"),
	gen_server:call(?MODULE, {tt1}).

tt1() -> 
	io:format("tt1~n"),   
	A = ibrowse:send_req("http://localhost:9393/download/FreeSWITCH-Air.air", 
		[], get, [], 
		[{stream_to, self()}, {stream_chunk_size, 4096}],
		9999999),
	{reply, A, "B"}.

download(Bucket, Key, Req) -> 
	io:format("download~n"),
	
	io:format("Req: ~p~n", [Req]),
	                 
	Path = Req:get(path), 
	
	io:format("Path: ~p~n", [Path]),
	
	
	TmpFile = ?TMP_PATH ++ "/" ++ helpers:uuid() ++ "_" ++ filename:basename(Path),
	
	io:format("Tmpfile: ~p~n", [TmpFile]),
	
	case filelib:is_file(TmpFile) of true ->
		io:format("Wanning: delete file before download ~p~n", [TmpFile]),
		file:delete(TmpFile);
		false -> ok
	end,
	
	case gen_server:call(s3, {stream, ?S3BUCKET, Key, [] } ) of
        {ibrowse_req_id, ReqId} ->
 			{ok, File} = file:open(TmpFile, write),
			ets:insert(stream_table, {ReqId, {File, Req}});
		Error -> io:format("Got error: ~p~n", [Error])
	end,
		
	{noreply, "OK"}.
