-module(earthwo_conf).

-export([ get/1
        , get/2
        ]).

%% OS Env > Config file

-spec(get(term()) -> {ok, term()} | {error, term()}).
get(Key) ->
    case os:getenv(Key) of
        false ->
            case application:get_env(earthwo, Key) of
                {ok, Val} -> {ok, Val};
                undefined -> {error, not_found}
            end;
        Val ->
            {ok, Val}
    end.

-spec(get(term(), term()) -> term()).
get(Key, Default) ->
    case os:getenv(Key) of
        false -> application:get_env(earthwo, Key, Default);
        Val -> Val
    end.
