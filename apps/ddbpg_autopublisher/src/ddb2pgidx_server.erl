%%%-------------------------------------------------------------------
%%% @author neerajsharma
%%% @copyright (C) 2017, neeraj.sharma@alumni.iitg.ernet.in
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ddb2pgidx_server).
-author("neerajsharma").

-behaviour(gen_server).

-include("ddbpg_autopublisher.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([receive_ddb_metrics/4]).

-define(SERVER, ?MODULE).

-record(state, {
    ddb_host :: undefined,
    ddb_port :: undefined | pos_integer(),
    bucket :: binary(),
    ref :: reference(),
    interval = 1000 :: pos_integer(),
    list_refresh_pid = undefined :: undefined | pid(),
    metrics = [] :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Index) ->
    lager:info("~p Index=~p", [?SERVER, Index]),
    {ok, DdbConfig} = application:get_env(?CORE_APPLICATION_NAME, ddbconfig),
    lager:info("~p DdbConfig=~p", [?SERVER, DdbConfig]),
    BucketConfig = lists:nth(Index, proplists:get_value(buckets, DdbConfig)),
    Name = proplists:get_value(name, BucketConfig),
    lager:info("~p starting actor ~p", [?SERVER, Name]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([BucketConfig]) ->
    process_flag(trap_exit, true),
    lager:info("~p config=~p", [?SERVER, BucketConfig]),
    case proplists:get_value(enabled, BucketConfig, false) of
        true ->
            {DDBHost, DDBPort} = proplists:get_value(ddb_endpoint, BucketConfig),
            DDBBucketS = proplists:get_value(ddb_bucket, BucketConfig),
            DDBBucket = list_to_binary(DDBBucketS),
            Interval = proplists:get_value(interval, BucketConfig),
            {ok, #state{interval = Interval, bucket = DDBBucket,
                ddb_host = DDBHost, ddb_port = DDBPort},
                0};
        false -> {ok, #state{}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({metrics, Bucket, Metrics}, State) ->
    %% TODO publish to pg-idx tables
    {noreply, State#state{metrics = Metrics}};
handle_cast(_Request, State) ->
    lager:debug("handle_cast(~p, ~p)", [_Request, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({timeout, _R, tick},
    #state{ref = _R, interval = FlushInterval, bucket = DDBBucket,
        ddb_host = DDBHost, ddb_port = DDBPort,
        list_refresh_pid = ListRefreshPid} = State) ->
    NewListRefreshPid = case is_pid(ListRefreshPid) of
                            true ->
                                case is_process_alive(ListRefreshPid) of
                                    true ->
                                        ListRefreshPid;
                                    false ->
                                        spawn_link(?MODULE, receive_ddb_metrics, [DDBHost, DDBPort, DDBBucket, self()])
                                end;
                            _ ->
                                spawn_link(?MODULE, receive_ddb_metrics, [DDBHost, DDBPort, DDBBucket, self()])
                            end,
    Ref = erlang:start_timer(FlushInterval, self(), tick),
    {noreply, State#state{ref = Ref,
        list_refresh_pid = NewListRefreshPid}};
handle_info(timeout, State = #state{ddb_host = Host, ddb_port = Port, bucket = Bucket,
    interval = Interval}) ->
    %% refresh the list immediately for local caching
    ListRefreshPid = spawn_link(node(), ?MODULE, receive_ddb_metrics, [Host, Port, Bucket, self()]),
    Ref = erlang:start_timer(Interval, self(), tick),
    {noreply, State#state{ref = Ref,
        list_refresh_pid = ListRefreshPid}};
handle_info({'EXIT', ListRefreshPid, _}, #state{list_refresh_pid = ListRefreshPid} = State) ->
    {noreply, State#state{list_refresh_pid = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


receive_ddb_metrics(Host, Port, Bucket, CallbackPid) ->
    lager:debug("trying to update metrics"),
    {ok, DDB} = ddb_tcp:connect(Host, Port),
    DDB2 = case ddb_tcp:list(Bucket, DDB) of
               {ok, Metrics, DDB1} ->
                   gen_server:cast(CallbackPid, {metrics, Bucket, Metrics}),
                   DDB1;
               {error, timeout, DDB1} -> DDB1
           end,
    ddb_tcp:close(DDB2).