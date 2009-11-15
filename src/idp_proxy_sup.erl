%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the idp_proxy application.

-module(idp_proxy_sup).
-author('author <author@example.com>').

-behaviour(supervisor).
  
-include("idp_proxy.hrl").   
-include("../deps/s3erl/include/s3.hrl").


%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
	
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,   
    WebConfig = [
         {ip, Ip},
                 {port, ?LISTEN_PORT},
                 {docroot, ?DOC_ROOT}],
    Web = {idp_proxy_web,
           {idp_proxy_web, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

	case filelib:is_dir(?DOC_ROOT) of false -> file:make_dir(?DOC_ROOT); true -> ok end,
	case filelib:is_dir(?TMP_PATH) of false -> file:make_dir(?TMP_PATH); true -> ok end, 
    
	S3Credentials = #aws_credentials{ accessKeyId=?S3AKI, secretAccessKey=?S3SAK },
	
	S3 = {s3,{s3, start, [S3Credentials]}, permanent, 5000, worker, dynamic},
	IBrowse = {ibrowse,{ibrowse, start, []}, permanent, 5000, worker, dynamic},
	StreamServer = {stream_server,{stream_server, start, []}, permanent, 5000, worker, dynamic},
	
    Processes = [Web,S3,IBrowse],
    {ok, {{one_for_one, 10, 10}, Processes}}.
