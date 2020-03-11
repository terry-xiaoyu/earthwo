%%%-------------------------------------------------------------------
%% @doc earthwo public API
%% @end
%%%-------------------------------------------------------------------

-module(earthwo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    earthwo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
