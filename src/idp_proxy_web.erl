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

	LocalPath = get_local_path(CleanPath),
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
			gen_server:cast(stream_server, {download, ?S3BUCKET, S3Key, Req});
		_ -> Req:not_found()
	end.
	
get_local_path("/" ++ Path) ->   
	filename:absname_join(?DOC_ROOT, Path).
	

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

