%%%-------------------------------------------------------------------
%%% @author neerajsharma
%%% @copyright (C) 2017, neeraj.sharma@alumni.iitg.ernet.in
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ddbpg_autopublisher_sup).
-author("neerajsharma").

-behaviour(supervisor).

-include("ddbpg_autopublisher.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Name, Index, Type), {Name, {I, start_link, [Index]}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    Response = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    dqe_idx_pg:init(),
    Response.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, DdbConfig} = application:get_env(?CORE_APPLICATION_NAME, ddbconfig),
    NumWorkers = length(proplists:get_value(buckets, DdbConfig)),
    Specs = [?CHILD(ddb2pgidx_server, proplists:get_value(name, lists:nth(Index, proplists:get_value(buckets, DdbConfig))), Index, worker) || Index <- lists:seq(1, NumWorkers)],
    {ok, { SupFlags, Specs} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

