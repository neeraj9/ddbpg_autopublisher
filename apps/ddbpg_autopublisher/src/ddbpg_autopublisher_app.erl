%%%-------------------------------------------------------------------
%% @doc ddbpg_autopublisher public API
%% @end
%%%-------------------------------------------------------------------

-module(ddbpg_autopublisher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    dqe_idx:init(),
    ddbpg_autopublisher_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
