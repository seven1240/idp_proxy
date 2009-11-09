%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(idp_proxy).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the idp_proxy server.
start() ->
    idp_proxy_deps:ensure(),
    ensure_started(crypto),
    application:start(idp_proxy).

%% @spec stop() -> ok
%% @doc Stop the idp_proxy server.
stop() ->
    Res = application:stop(idp_proxy),
    application:stop(crypto),
    Res.
