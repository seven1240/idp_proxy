%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the idp_proxy application.

-module(idp_proxy_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for idp_proxy.
start(_Type, _StartArgs) ->
    idp_proxy_deps:ensure(),
    idp_proxy_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for idp_proxy.
stop(_State) ->
    ok.
