%% @copyright 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc エントリへのリンクや監視が行えるKey-Valueテーブル
%%
%% TODO: uesr => peer(?)
-module(leakless_table).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_opt/1]).
-export([store/3, store_connect/5]).
-export([find/2]).
-export([erase/2]).
-export([to_list/1]).
-export([connect/4, disconnect/2, disconnect_all/3]).
-export([flush/1, flush/3]).

-export_type([table_ref/0, key/0, value/0]).
-export_type([connection/0, edge_spec/0, edge_type/0, peer/0]).
-export_type([exit_message/0, down_message/0, disconnect_info/0, disconnect_reason/0]).
-export_type([start_option/0, start_options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-record(state,
        {
          table = gb_trees:empty() :: table(),
          edges = gb_trees:empty() :: edges()
        }).

-record(entry,
        {
          value                    :: value(),
          peers = gb_trees:empty() :: peers()
        }).

-type edge_spec() :: {edge_type(), peer(), key()}.
-type edge_type() :: link | forward_link | backward_link | monitor.
-type peer() :: pid().
-type connection() :: reference().

-type table() :: gb_trees:tree(key(), #entry{}).
-type edges() :: gb_trees:tree(connection(), edge_spec()).
-type peers() :: gb_trees:tree(peer(), [connection()]).

-type key() :: term().
-type value() :: term().

-type exit_message() :: {'EXIT', pid(), {'DISCONNECTED', disconnect_info()}}.
-type down_message() :: {'DOWN', connection(), leakless_table_entry, key(), {'DISCONNECTED', disconnect_info()}}.
-type disconnect_info() :: #{table      => {?MODULE, pid()},
                             edge_spec  => edge_spec(),
                             connection => connection(),
                             reason     => disconnect_reason()}.

-type disconnect_reason() :: {'EXIT', peer(), Reason::term()}
                           | {erased, Eraser::pid()}
                           | noentry
                           | {table_down, Reason::term()}.

-type table_ref() :: leakless:otp_proc_ref().

-type start_options() :: [start_option()].
-type start_option() :: link
                      | {name, leakless:otp_proc_name()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec start_opt(start_options()) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_opt(Options) ->
    SpawnOptions =
        case lists:member(link, Options) of
            true  -> [link];
            false -> []
        end,
    case proplists:get_value(name, Options, undefined) of
        undefined -> gen_server:start(?MODULE, [], [{spawn_opt, SpawnOptions}]);
        Name      -> gen_server:start(Name, ?MODULE, [], [{spawn_opt, SpawnOptions}])
    end.

-spec store(table_ref(), key(), value()) -> ok | {error, Reason} when
      Reason :: {already_exists, value()}.
store(Table, Key, Value) ->
    gen_server:call(Table, {store, {Key, Value}}).

-spec store_connect(table_ref(), edge_type(), peer(), key(), value()) -> {ok, connection()} | {error, Reason} when
      Reason :: {already_exists, value()}.
store_connect(Table, Type, Peer, Key, Value) when is_pid(Peer) ->
    _ = lists:member(Type, edge_types()) orelse error(badarg, [Table, Type, Peer, Key, Value]),
    gen_server:call(Table, {store_connect, {{Table, Peer, Key}, Value}}).

-spec find(table_ref(), key()) -> {ok, value()} | error.
find(Table, Key) ->
    gen_server:call(Table, {find, Key}).

-spec erase(table_ref(), key()) -> ok.
erase(Table, Key) ->
    gen_server:cast(Table, {erase, {self(), Key}}).

-spec to_list(table_ref()) -> [{key(), value()}].
to_list(Table) ->
    gen_server:call(Table, to_list).

-spec connect(table_ref(), edge_type(), peer(), key()) -> connection().
connect(Table, Type, Peer, Key) when is_pid(Peer) ->
    _ = lists:member(Type, edge_types()) orelse error(badarg, [Table, Type, Peer, Key]),
    gen_server:call(Table, {connect, {Type, Peer, Key}}).

-spec disconnect(table_ref(), connection()) -> ok.
disconnect(Table, Connection) ->
    ok = gen_server:call(Table, {disconnect, Connection}),
    flush(Connection).

-spec disconnect_all(table_ref(), peer(), key()) -> [connection()].
disconnect_all(Table, Peer, Key) when is_pid(Peer) ->
    Connections = gen_server:call(Table, {disconnect_all, {Peer, Key}}),
    ok = flush(Table, Peer, Key), % XXX: ローカルのメールボックスしかフラッシュしないから Peer =/= self()の場合はあまり意味がない
    Connections.

-spec flush(connection()) -> ok.
flush(Connection) ->
    receive
        {'EXIT', _, {_, #{connection := Connection}}} -> ok;
        {'DOWN', Connection, _, _, _}                 -> ok
    after 0 -> ok
    end.

-spec flush(table_ref(), peer(), key()) -> ok.
flush(Table, Peer, Key) ->
    case where(Table) of
        undefined -> ok;
        TablePid  ->
            Recur =
                fun Recur() ->
                        receive
                            {'EXIT', TablePid, {'DISCONNECTED', #{edge_spec := {_, Peer, Key}}}} -> Recur();
                            {'DOWN', _, _, _, {'DISCONNECTED', #{table := {_, TablePid}, edge_spec := {_, Peer, Key}}}} -> Recur()
                        after 0 -> ok
                        end
                end,
            Recur()
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    _ = process_flag(trap_exit, true),
    State = #state{},
    {ok, State}.

%% @private
handle_call({store, Arg}, _From, State)          -> handle_store(Arg, State);
handle_call({store_connect, Arg}, _From, State)  -> handle_store_connect(Arg, State);
handle_call({find,  Arg}, _From, State)          -> handle_find(Arg, State);
handle_call({connect, Arg}, _From, State)        -> handle_connect(Arg, State);
handle_call({disconnect, Arg}, _From, State)     -> handle_disconnect(Arg, State);
handle_call({disconnect_all, Arg}, _From, State) -> handle_disconnect_all(Arg, State);
handle_call(to_list, _From, State)               -> handle_to_list(State);
handle_call(Request, From, State)                -> {stop, {unknown_call, Request, From}, State}.

%% @private
handle_cast({erase, Arg}, State) -> handle_erase(Arg, State);
handle_cast(Request, State)      -> {stop, {unknown_cast, Request}, State}.

%% @private
handle_info({'DOWN', Ref, _, Pid, Reason}, State) -> handle_down(Ref, Pid, Reason, State);
handle_info({'EXIT',_Pid, normal}, State)         -> {noreply, State};
handle_info({'EXIT',_Pid, Reason}, State)         -> {stop, Reason, State};
handle_info(Info, State)                          -> {stop, {unknown_info, Info}, State}.

%% @private
terminate(Reason, State) ->
    _ = lists:foldl(fun (Key, Acc) -> do_erase(Key, {table_down, Reason}, Acc) end,
                    State,
                    gb_trees:keys(State#state.table)),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec edge_types() -> [edge_type()].
edge_types() -> [link, forward_link, backward_link, monitor].

-spec handle_store({key(), value()}, #state{}) -> {reply, Result, #state{}} when
      Result :: ok | {error, {already_exists, value()}}.
handle_store({Key, Value}, State0) ->
    case gb_trees:lookup(Key, State0#state.table) of
        {_, E} -> {reply, {error, {already_exists, E#entry.value}}, State0};
        none   ->
            Table  = gb_trees:insert(Key, #entry{value = Value}, State0#state.table),
            State1 = State0#state{table = Table},
            {reply, ok, State1}
    end.

-spec handle_store_connect({edge_spec(), value()}, #state{}) -> {reply, Result, #state{}} when
      Result :: {ok, connection()} | {error, {already_exists, value()}}.
handle_store_connect({EdgeSpec = {_, _, Key}, Value}, State0) ->
    case handle_store({Key, Value}, State0) of
        {_, {error, _}, _} = ErrorReply -> ErrorReply;
        {_, ok, State1}                 ->
            {_, Connection, State2} = handle_connect(EdgeSpec, State1),
            {reply, {ok, Connection}, State2}
    end.

-spec handle_find(key(), #state{}) -> {reply, Result, #state{}} when
      Result :: {ok, value()} | error.
handle_find(Key, State) ->
    case gb_trees:lookup(Key, State#state.table) of
        {_, E} -> {reply, {ok, E#entry.value}, State};
        none   -> {reply, error, State}
    end.

-spec handle_connect(edge_spec(), #state{}) -> {reply, connection(), #state{}}.
handle_connect(EdgeSpec = {_, _, Key}, State0) ->
    case gb_trees:lookup(Key, State0#state.table) of
        none ->
            Connecion = make_ref(),
            ok = do_disconnect(Connecion, EdgeSpec, noentry),
            {reply, Connecion, State0};
        {_, Entry0} ->
            {Entry1, Edges, Connection} = do_connect(EdgeSpec, Entry0, State0#state.edges),
            Table = gb_trees:update(Key, Entry1, State0#state.table),
            State1 = State0#state{table = Table, edges = Edges},
            {reply, Connection, State1}
    end.

-spec handle_disconnect(connection(), #state{}) -> {reply, ok, #state{}}.
handle_disconnect(Connection, State0) ->
    case gb_trees:lookup(Connection, State0#state.edges) of
        none                    -> {reply, ok, State0};
        {_, {_Type, Peer, Key}} ->
            _ = erlang:demonitor(Connection),

            Entry0 = gb_trees:get(Key, State0#state.table),
            Peers = case lists:delete(Connection, gb_trees:get(Peer, Entry0#entry.peers)) of
                        []          -> gb_trees:delete(Peer, Entry0#entry.peers);
                        Connections -> gb_trees:update(Peer, Connections, Entry0#entry.peers)
                    end,
            Entry1 = Entry0#entry{peers = Peers},

            Edges = gb_trees:delete(Connection, State0#state.edges),
            Table = gb_trees:update(Key, Entry1, State0#state.table),

            State1 = State0#state{table = Table, edges = Edges},
            {reply, ok, State1}
    end.

-spec handle_disconnect_all({peer(), key()}, #state{}) -> {reply, [connection()], #state{}}.
handle_disconnect_all({Peer, Key}, State0) ->
    case gb_trees:lookup(Key, State0#state.table) of
        none        -> {reply, [], State0};
        {_, Entry0} ->
            case gb_trees:lookup(Peer, Entry0#entry.peers) of
                none             -> {reply, [], State0};
                {_, Connections} ->
                    Entry1 = Entry0#entry{peers = gb_trees:delete(Peer, Entry0#entry.peers)},
                    Table = gb_trees:update(Key, Entry1, State0#state.table),
                    Edges = do_remove_connections(Connections, State0#state.edges),
                    State1 = State0#state{table = Table, edges = Edges},
                    {reply, Connections, State1}
            end
    end.

-spec handle_erase({pid(), key()}, #state{}) -> {noreply, #state{}}.
handle_erase({Caller, Key}, State) ->
    {noreply, do_erase(Key, {erased, Caller}, State)}.

-spec do_disconnect(connection(), edge_spec(), disconnect_reason()) -> ok.
do_disconnect(Connection, Spec = {Type, Peer, Key}, Reason) ->
    Info = #{table => {?MODULE, self()}, edge_spec => Spec, connection => Connection, reason => Reason},
    _ = case Type of
            backward_link -> ok;
            monitor       -> Peer ! {'DOWN', Connection, leakless_table_entry, Key, {'DISCONNECTED', Info}};
            _             -> exit(Peer, {'DISCONNECTED', Info})
        end,
    ok.

-spec do_remove_connections([connection()], edges()) -> edges().
do_remove_connections(Connections, Edges) ->
    lists:foldl(fun do_remove_connection/2, Edges, Connections).

-spec do_remove_connection(connection(), edges()) -> edges().
do_remove_connection(Connection, Edges) ->
    _ = erlang:demonitor(Connection),
    gb_trees:delete(Connection, Edges).

-spec do_erase(key(), disconnect_reason(), #state{}) -> #state{}.
do_erase(Key, Reason, State) ->
    case gb_trees:lookup(Key, State#state.table) of
        none       -> State;
        {_, Entry} ->
            Table = gb_trees:delete(Key, State#state.table),
            Edges =
                lists:foldl(
                  fun (Connections, Acc0) ->
                          lists:foldl(fun (Connection, Acc1) ->
                                              Acc2 = do_remove_connection(Connection, Acc1),
                                              do_disconnect(Connection, gb_trees:get(Connection, Acc1), Reason),
                                              Acc2
                                      end,
                                      Acc0,
                                      Connections)
                  end,
                  State#state.edges,
                  gb_trees:values(Entry#entry.peers)),
            State#state{table = Table, edges = Edges}
    end.

-spec handle_down(connection(), pid(), term(), #state{}) -> {noreply, #state{}}.
handle_down(Connection, Pid, Reason, State) ->
    case gb_trees:lookup(Connection, State#state.edges) of
        none                         -> {noreply, State};
        {_, {backward_link, _, Key}} -> {noreply, do_erase(Key, {'EXIT', Pid, Reason}, State)};
        {_, {link,          _, Key}} -> {noreply, do_erase(Key, {'EXIT', Pid, Reason}, State)};
        {_, _}                       -> {noreply, element(3, handle_disconnect(Connection, State))}
    end.

-spec handle_to_list(#state{}) -> {reply, [{key(), value()}], #state{}}.
handle_to_list(State) ->
    {reply, [{K, V} || {K, #entry{value = V}} <- gb_trees:to_list(State#state.table)], State}.

-spec do_connect(edge_spec(), #entry{}, edges()) -> {#entry{}, edges(), connection()}.
do_connect(Spec = {_, Peer, _}, Entry0, Edges0) ->
    Connection = erlang:monitor(process, Peer),
    Peers =
        case gb_trees:lookup(Peer, Entry0#entry.peers) of
            none             -> gb_trees:insert(Peer, [Connection], Entry0#entry.peers);
            {_, Connections} -> gb_trees:update(Peer, [Connection | Connections], Entry0#entry.peers)
        end,
    Entry1 = Entry0#entry{peers = Peers},
    Edges1 = gb_trees:insert(Connection, Spec, Edges0),
    {Entry1, Edges1, Connection}.

-spec where(table_ref()) -> pid() | undefined.
where({global, Name})          -> global:whereis_name(Name);
where({via, Module, Name})     -> Module:whereis_name(Name);
where(Name) when is_atom(Name) -> whereis(Name);
where(Pid) when is_pid(Pid)    -> Pid.
