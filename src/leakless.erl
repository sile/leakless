%% @copyright 2015, Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 共通的に使われる関数や型を定義しているモジュール
-module(leakless).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export_type([otp_proc_name/0, otp_proc_ref/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type otp_proc_name() :: {local, Name :: atom()}
                       | {global, Name :: term()}
                       | {via, module(), Name :: term()}.
%% {@link gen_server:start/4}等で指定可能な起動プロセスの名前

-type otp_proc_ref() :: (Name :: atom())
                      | {Name :: atom(), node()}
                      | {global, Name :: term()}
                      | {via, module(), Name :: term()}
                      | pid().
%% {@link gen_server:cast/2}等で指定可能な宛先プロセスの参照