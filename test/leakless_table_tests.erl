%% coding: latin-1
%%
%% @copyright 2015, Takeru Ohta <phjgt308@gmail.com>
-module(leakless_table_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(assignMatch(Pattern, Exp),
        Pattern =
            (fun () ->
                     case Exp of
                         Pattern -> Pattern;
                         __V     ->
                             error({assertMatch_failed,
                                    [{module, ?MODULE},
                                     {line, ?LINE},
                                     {expression, (??Exp)},
                                     {pattern, (??Pattern)},
                                     {value, __V}]})
                     end
             end)()).

-define(assertReceive(Expected),
        receive
            Expected -> ?assert(true)
        after 25 -> ?assert(timeout)
        end).
-define(assertReceiveNot(UnExpected),
        receive
            UnExpected -> ?assert({unexpected_message, ??UnExpected})
        after 25 -> ?assert(true)
        end).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
start_test_() ->
    Name = ?MODULE,
    [
     {"プロセスが起動できる",
      fun () ->
              ?assignMatch({ok, Pid}, leakless_table:start_opt([])),
              exit(Pid, kill)
      end},
     {"プロセスがlink付きで起動できる",
      fun () ->
              process_flag(trap_exit, true),
              ?assignMatch({ok, Pid}, leakless_table:start_opt([link])),
              exit(Pid, kill),
              ?assertReceive({'EXIT', Pid, killed}),
              process_flag(trap_exit, false)
      end},
     {"名前付きプロセスが起動できる",
      fun () ->
              ?assertEqual(undefined, whereis(Name)),
              ?assignMatch({ok, Pid}, leakless_table:start_opt([{name, {local, Name}}])),
              ?assertEqual(Pid, whereis(Name)),
              exit(Pid, kill)
      end},
     {"同じ名前のプロセスは重複起動できない",
      fun () ->
              {ok, Pid} = leakless_table:start_opt([{name, {local, Name}}]),
              ?assertEqual({error, {already_started, Pid}}, leakless_table:start_opt([{name, {local, Name}}])),
              exit(Pid, kill)
      end}
    ].

basic_store_and_erase_test_() ->
    Table = ?MODULE,
    {foreach,
     fun ()  -> table_setup(Table) end,
     fun (_) -> table_tear_down(Table) end,
     [
      {"エントリが追加できる",
       fun () ->
               ?assertEqual(ok, leakless_table:store(Table, key, value)),
               ?assertEqual({ok, value}, leakless_table:find(Table, key))
       end},
      {"同じキーを持つエントリが既に存在する場合は、追加には失敗する",
       fun () ->
               ?assertEqual(ok, leakless_table:store(Table, key, value1)),
               ?assertEqual({error, {already_exists, value1}}, leakless_table:store(Table, key, value2))
       end},
      {"エントリが削除できる",
       fun () ->
               ok = leakless_table:store(Table, key, value),
               {ok, value} = leakless_table:find(Table, key),
               ?assertEqual(ok, leakless_table:erase(Table, key)),
               ?assertEqual(error, leakless_table:find(Table, key))
       end},
      {"存在しないエントリに対する削除呼び出しは単に無視される",
       fun () ->
               error = leakless_table:find(Table, key),
               ?assertEqual(ok, leakless_table:erase(Table, key))
       end}
     ]}.

to_list_test_() ->
    Table = ?MODULE,
    {foreach,
     fun ()  -> table_setup(Table) end,
     fun (_) -> table_tear_down(Table) end,
     [
      {"テーブルの内容がリストに変換できる",
       fun () ->
               ok = leakless_table:store(Table, key1, value1),
               ok = leakless_table:store(Table, key2, value2),
               ?assertEqual([{key1, value1}, {key2, value2}],
                            lists:sort(leakless_table:to_list(Table)))
       end}
     ]}.

connect_test_() ->
    Table = ?MODULE,
    {foreach, spawn,
     fun ()  -> table_setup(Table) end,
     fun (_) -> table_tear_down(Table) end,
     [
      {"[link] エントリにlink接続が行える",
       fun () ->
               process_flag(trap_exit, true),
               ok = leakless_table:store(Table, key, value),
               C = leakless_table:connect(Table, link, self(), key),

               ok = leakless_table:erase(Table, key),
               Self = self(),
               ?assertReceive({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := {erased, Self}}}})
       end},
      {"[link] 接続元プロセスが死んだ場合は、エントリは削除される",
       fun () ->
               process_flag(trap_exit, true),
               ok = leakless_table:store(Table, key, value),
               Self = self(),
               Pid = spawn(timer, sleep, [infinity]),
               _ = leakless_table:connect(Table, link, Pid, key),
               C = leakless_table:connect(Table, link, Self, key),

               exit(Pid, kill),  % linkプロセスの内の一つを殺す

               ?assertReceive({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := {'EXIT', Pid, killed}}}}),
               ?assertEqual(error, leakless_table:find(Table, key))
       end},
      {"[link] 接続先エントリが存在しない場合でも、接続自体は成功する(ただし即座にexit/2が呼ばれる)",
       fun () ->
               process_flag(trap_exit, true),
               C = leakless_table:connect(Table, link, self(), key),
               ?assertReceive({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := noentry}}})
       end},
      {"[forward_link] エントリにforward_link接続が行える",
       fun () ->
               process_flag(trap_exit, true),
               ok = leakless_table:store(Table, key, value),
               C = leakless_table:connect(Table, forward_link, self(), key),

               ok = leakless_table:erase(Table, key),
               Self = self(),
               ?assertReceive({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := {erased, Self}}}})
       end},
      {"[forward_link] 接続元プロセスが死んだ場合でも、エントリは削除されない",
       fun () ->
               process_flag(trap_exit, true),
               ok = leakless_table:store(Table, key, value),
               Self = self(),
               Pid = spawn(timer, sleep, [infinity]),
               _ = leakless_table:connect(Table, forward_link, Pid, key),
               C = leakless_table:connect(Table, link, Self, key),

               exit(Pid, kill),  % linkプロセスの内の一つを殺す

               ?assertReceiveNot({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := {'EXIT', Pid, killed}}}}),
               ?assertEqual({ok, value}, leakless_table:find(Table, key))
       end},
      {"[forward_link] リンク先エントリが存在しない場合でも、接続自体は成功する(ただし即座にexit/2が呼ばれる)",
       fun () ->
               process_flag(trap_exit, true),
               C = leakless_table:connect(Table, forward_link, self(), key),
               ?assertReceive({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := noentry}}})
       end},
      {"[backward_link] エントリにbackward_link接続が行える",
       fun () ->
               ok = leakless_table:store(Table, key, value),
               C = leakless_table:connect(Table, backward_link, self(), key),

               ok = leakless_table:erase(Table, key),
               Self = self(),
               ?assertReceiveNot({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := {erased, Self}}}})
       end},
      {"[backward_link] 接続元プロセスが死んだ場合は、エントリは削除される",
       fun () ->
               process_flag(trap_exit, true),
               ok = leakless_table:store(Table, key, value),
               Self = self(),
               Pid = spawn(timer, sleep, [infinity]),
               _ = leakless_table:connect(Table, backward_link, Pid, key),
               C = leakless_table:connect(Table, link, Self, key),

               exit(Pid, kill),  % linkプロセスの内の一つを殺す

               ?assertReceive({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := {'EXIT', Pid, killed}}}}),
               ?assertEqual(error, leakless_table:find(Table, key))
       end},
      {"[backward_link] リンク先エントリが存在しない場合でも、接続自体は成功する(ただし即座に解除される)",
       fun () ->
               C = leakless_table:connect(Table, backward_link, self(), key),
               ?assertReceiveNot({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := noentry}}}),
               ?assertEqual([], leakless_table:disconnect_all(Table, self(), key))
       end},
      {"[monitor] エントリにmonitor接続が行える",
       fun () ->
               ok = leakless_table:store(Table, key, value),
               C = leakless_table:connect(Table, monitor, self(), key),

               ok = leakless_table:erase(Table, key),
               Self = self(),
               ?assertReceive({'DOWN', C, _, _, {'DISCONNECTED', #{reason := {erased, Self}}}})
       end},
      {"[monitor] リンク先エントリが存在しない場合でも、接続自体は成功する(ただし即座に通知メッセージが飛んで来る)",
       fun () ->
               C = leakless_table:connect(Table, monitor, self(), key),
               ?assertReceive({'DOWN', C, _, _, {'DISCONNECTED', #{reason := noentry}}})
       end},
      {"接続は解除できる",
       fun () ->
               ok = leakless_table:store(Table, key, value),
               C  = leakless_table:connect(Table, link, self(), key),
               ok = leakless_table:disconnect(Table, C),

               ok = leakless_table:erase(Table, key),
               Self = self(),
               ?assertReceiveNot({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := {erased, Self}}}})
       end},
      {"存在しない接続に対する解除要求は単に霧視される",
       fun () ->
               ?assertEqual(ok, leakless_table:disconnect(Table, make_ref()))
       end},
      {"接続は重複して行うことが可能",
       fun () ->
               process_flag(trap_exit, true),
               ok = leakless_table:store(Table, key, value),
               C0 = leakless_table:connect(Table, link, self(), key),
               C1 = leakless_table:connect(Table, link, self(), key),
               C2 = leakless_table:connect(Table, monitor, self(), key),
               ok = leakless_table:disconnect(Table, C0), % C0だけ解除

               ok = leakless_table:erase(Table, key),
               Self = self(),
               ?assertReceive({'EXIT', _, {'DISCONNECTED', #{connection := C1, reason := {erased, Self}}}}),
               ?assertReceive({'DOWN', C2, _, _, {'DISCONNECTED', #{reason := {erased, Self}}}})
       end},
      {"エントリとプロセス間の接続群は一括解除が可能",
       fun () ->
               ok = leakless_table:store(Table, key, value),
               C0 = leakless_table:connect(Table, link, self(), key),
               C1 = leakless_table:connect(Table, link, self(), key),
               C2 = leakless_table:connect(Table, monitor, self(), key),

               %% 一括解除
               ?assertEqual(
                  lists:sort([C0, C1, C2]),
                  lists:sort(leakless_table:disconnect_all(Table, self(), key))),

               ok = leakless_table:erase(Table, key),
               Self = self(),
               ?assertReceiveNot({'EXIT', _, {'DISCONNECTED', #{connection := C0, reason := {erased, Self}}}}),
               ?assertReceiveNot({'EXIT', _, {'DISCONNECTED', #{connection := C1, reason := {erased, Self}}}}),
               ?assertReceiveNot({'DOWN', C2, _, _, {'DISCONNECTED', #{reason := {erased, Self}}}})
       end},
      {"解除時にメールボックスに溜まっている通知メッセージはフラッシュされる (1)",
       fun () ->
               process_flag(trap_exit, true),
               ok = leakless_table:store(Table, key, value),
               C0 = leakless_table:connect(Table, link, self(), key),
               C1 = leakless_table:connect(Table, monitor, self(), key),

               ok = leakless_table:erase(Table, key),
               _ = timer:sleep(20),
               _ = leakless_table:disconnect_all(Table, self(), key),

               Self = self(),
               ?assertReceiveNot({'EXIT', _, {'DISCONNECTED', #{connection := C0, reason := {erased, Self}}}}),
               ?assertReceiveNot({'DOWN', C1, _, _, {'DISCONNECTED', #{reason := {erased, Self}}}})
       end},
      {"解除時にメールボックスに溜まっている通知メッセージはフラッシュされる (2)",
       fun () ->
               process_flag(trap_exit, true),
               ok = leakless_table:store(Table, key, value),
               C0 = leakless_table:connect(Table, link, self(), key),
               C1 = leakless_table:connect(Table, monitor, self(), key),

               ok = leakless_table:erase(Table, key),
               _ = timer:sleep(20),
               _ = leakless_table:disconnect(Table, C0),
               _ = leakless_table:disconnect(Table, C1),

               Self = self(),
               ?assertReceiveNot({'EXIT', _, {'DISCONNECTED', #{connection := C0, reason := {erased, Self}}}}),
               ?assertReceiveNot({'DOWN', C1, _, _, {'DISCONNECTED', #{reason := {erased, Self}}}})
       end},
      {"エントリの登録と接続はアトミックに行える",
       fun () ->
               process_flag(trap_exit, true),
               ?assignMatch({ok, C}, leakless_table:store_connect(Table, link, self(), key, value)),

               ok = leakless_table:erase(Table, key),
               Self = self(),
               ?assertReceive({'EXIT', _, {'DISCONNECTED', #{connection := C, reason := {erased, Self}}}})
       end}
     ]}.

misc_test_() ->
     [
     {"不明なメッセージを受け取ると終了する",
      fun () ->
              process_flag(trap_exit, true),
              {ok, Pid0} = leakless_table:start_opt([link]),
              ?assertExit({{unknown_call, hoge, _}, _}, gen_server:call(Pid0, hoge)),
              ?assertReceive({'EXIT', Pid0, {unknown_call, hoge, _}}),

              {ok, Pid1} = leakless_table:start_opt([link]),
              ok = gen_server:cast(Pid1, hoge),
              ?assertReceive({'EXIT', Pid1, {unknown_cast, hoge}}),

              {ok, Pid2} = leakless_table:start_opt([link]),
              _ = Pid2 ! hoge,
              ?assertReceive({'EXIT', Pid2, {unknown_info, hoge}})
      end},
      {"linkプロセスが異常終了した場合は、テーブルプロセスも終了する",
       fun () ->
               process_flag(trap_exit, true),
               {ok, Pid} = leakless_table:start_opt([link]),

               spawn(fun () -> link(Pid) end), % 正常終了
               ?assertReceiveNot({'EXIT', Pid, _}),

               spawn(fun () -> link(Pid), exit(hoge) end), % 異常終了
               ?assertReceive({'EXIT', Pid, hoge})
       end}
    ].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec table_setup(atom()) -> ok.
table_setup(Table) ->
    {ok, _} = leakless_table:start_opt([{name, {local, Table}}]),
    ok.

-spec table_tear_down(atom()) -> ok.
table_tear_down(Table) ->
    case whereis(Table) of
        undefined -> ok;
        Pid       ->
            unregister(Table),
            exit(Pid, kill),
            ok
    end.
