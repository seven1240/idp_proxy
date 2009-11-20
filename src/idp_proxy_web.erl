%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for idp_proxy.

-module(idp_proxy_web).
-author('author <author@example.com>').

-export([start/1, stop/0, rest_response/1]).   

-include("idp_proxy.hrl").   

	
%% External API                   


start(Options) ->
                                                                   
    {_DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:rest_response(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).
                                         
rest_response(Req, 'GET', "/") ->
 	Req:respond({200, [], "Hello"});
    
rest_response(Req, 'GET', Path) ->
	io:format("~p~n", [Path]),
    serve_file(Req, Path);

rest_response(Req, 'HEAD', Path) ->
    serve_file(Req, Path);

rest_response(Req, 'POST', _Path) ->
    Req:respond({501, [], []}).
	
rest_response(Req) ->
    rest_response(Req, Req:get(method), Req:get(path)).

serve_file(Req, Path) ->          

	% nginx send request from a internal location which using for send_file,
	% but it preserves the original Path, so we had to add a custom header
	% X-Uri to locate file in the local file-internal path
	
	XUri = case Req:get_header_value("x-uri") of
		undefined -> Path;
		Uri -> Uri
	end,
	
	io:format("Internal Path: ~p~n", [XUri]),
	{ok, CleanPath, _} = regexp:sub(XUri, ?INTERNAL_PATH, ""), 

	LocalPath = get_local_path(Req),
	io:format("Local Path: ~p~n", [LocalPath]),
	
	case filelib:is_file(LocalPath) of
		true -> 
			io:format("found file ~p ~n", [LocalPath]),
			"/" ++ RPath = CleanPath,
			Req:serve_file(RPath, ?DOC_ROOT);
		false ->
			fetch_from_s3_and_serve(Req, CleanPath)
	end.
		
fetch_from_s3_and_serve(Req, Path) ->

	"/" ++ RPath = Path,

	S3Key = ?S3PREFIX ++ RPath,	
	S3Url = ?S3BUCKET ++ ":" ++ S3Key,
	io:format("S3Url: ~p~n", [S3Url]),
	
	case s3:has_key(?S3BUCKET, S3Key) of
		true ->			
			download_from_s3(Req, S3Key);
			% gen_server:cast(stream_server, {download, ?S3BUCKET, S3Key, Req});
		_ -> Req:not_found()
	end.
	
download_from_s3(Req, Key) ->
	io:format("download~n"),
	io:format("Req: ~p~n", [Req]),
	                 
	Path = Req:get(path), 
	
	io:format("Path: ~p~n", [Path]),
	
	
	TmpFile = ?TMP_PATH ++ "/" ++ get_pid_str(self()) ++ "_" ++ filename:basename(Path),
	
	io:format("Tmpfile: ~p~n", [TmpFile]),
	
	case filelib:is_file(TmpFile) of true ->
		io:format("Wanning: delete file before download ~p~n", [TmpFile]),
		file:delete(TmpFile);
		false -> ok
	end,
	
	% put to process dictionary
	put(tmpfile, TmpFile),
	put(request, Req),
	
	% case ibrowse:send_req("http://localhost:3000/open-flash-chart-bar-clicking.swf",
	% 	[], get, [], 
	% 	[{stream_to, self()}, {stream_chunk_size, 4096}],
	% 	9999999) of
	% 	
	case gen_server:call(s3, {stream, ?S3BUCKET, Key, [] } ) of
		
		{ibrowse_req_id, ReqId} -> receive_loop(Req, ReqId);
		Error -> 
			io:format("Error sending request to S3 ~p~n", [Error]),
			Req:respond({500, [], "Sorry something went wrong!"})
	end.

receive_loop(Req, ReqId) ->
	receive
		{ibrowse_async_headers, ReqId, Code, Headers} ->
			handle_async_headers(ReqId, Code, Headers),
			receive_loop(Req, ReqId);
		{ibrowse_async_response, ReqId, Body} ->
			handle_async_response(ReqId, Body),
			receive_loop(Req, ReqId);
		{ibrowse_async_response_end, ReqId} ->
            handle_async_response_end(ReqId);
			
		X -> io:format("Got something else ~p~n", [X]),
			receive_loop(Req, ReqId)
	after 8000 ->
		io:format("timeout~n"),
		Req:not_found()
	end.


handle_async_headers(ReqId, Code, Headers) ->
		% io:format("async_header~p~n", [Headers]),       
		ContentType = case proplists:get_value("Content-Type", Headers) of
			[] -> "application/octet-stream";
			CT -> CT
		end,
		ContentLength = proplists:get_value("Content-Length", Headers),
		Req = get(request),
		Res = Req:ok({ContentType, [{"Content-Length", ContentLength}], chunked}),
		put(res, Res),
		TmpFile = get(tmpfile),
		{ok, FileHandle} = file:open(TmpFile, write),
		put(tmpfile_handle, FileHandle).
		
handle_async_response(ReqId, Body) ->
	io:format("response, reqid: ~p ", [ReqId]), 
	FileHandle = get(tmpfile_handle),
	Res = get(res),
	file:write(FileHandle, Body),
	Res:write_chunk(Body),
	io:format("Chunk written~n"), 
	
	ibrowse:stream_next(ReqId).

handle_async_response_end(ReqId) ->
		io:format("end: ~n"),
		FileHandle = get(tmpfile_handle),
		case get(res) of 
			Res -> Res:write_chunk("");
			_ -> noop
		end,
		
		file:close(FileHandle),
		
		Req = get(request),
		LocalPath = get_local_path(Req),

		case filelib:is_dir(LocalPath) of
			false -> os:cmd("mkdir -p " ++ filename:dirname(LocalPath));
			true -> ok
		end,                                                             

		io:format("Move file to the right place: ~p~n", ["mv " ++ " " ++ LocalPath]).
		% os:cmd("mv " ++ TmpFile ++ " " ++ LocalPath),     

%% Internal API

get_local_path(Req) ->
	"/" ++ RPath = Req:get(path),
	filename:absname_join(?DOC_ROOT, RPath).
	
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

get_pid_str(Pid) ->
	string:strip(string:strip(pid_to_list(Pid), right, $>), left, $<).