
%%% erlyberly, erlang trace debugger
%%% Copyright (C) 2016 Andy Till
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(erlyberly).

-export([collect_seq_trace_logs/0]).
-export([collect_trace_logs/0]).
-export([ensure_dbg_started/2]).
-export([ensure_xref_started/0]).
-export([saleyn_fun_src/1]).
-export([get_abstract_code/1]).
-export([get_process_state/1]).
-export([get_source_code/1]).
-export([load_modules_on_path/1]).
-export([module_functions/0]).
-export([process_info/0]).
-export([seq_trace/5]).
-export([start_trace/5]).
-export([stop_trace/4]).
-export([stop_traces/0]).
-export([xref_analysis/4]).

%% exported for spawned processes
-export([erlyberly_tcollector/3]).

%%% ============================================================================
%%% gen_event function exports
%%% ============================================================================

-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%% ============================================================================
%%% process info
%%% ============================================================================

%% Asynchronously load all modules on the path where the app directory matches
%% the given regular expression.
load_modules_on_path(RegexValidator) ->
    proc_lib:spawn(
        fun() -> load_modules_on_path2(RegexValidator) end).

%%
load_modules_on_path2(RegexValidator) ->
    Paths = [P || P <- code:get_path(), re:run(P, RegexValidator) /= nomatch],
    [code:ensure_loaded(file_name_to_module(F)) || P <- Paths,
                                                   F <- filelib:wildcard(P ++ "/*.beam")].

%%
file_name_to_module(Filename) ->
    list_to_atom(filename:rootname(filename:basename(Filename))).

process_info() ->
    process_info2(erlang:processes(), []).

process_info2([], Acc) ->
    Acc;
process_info2([undefined | Tail], Acc) ->
    process_info2(Tail, Acc);
process_info2([Proc | Tail], Acc) ->
    Props = erlang:process_info(Proc, [registered_name,
                                       reductions,
                                       message_queue_len,
                                       heap_size,
                                       stack_size,
                                       total_heap_size]),
    Acc1 = case Props of
               undefined -> 
                   Acc;
               _ -> 
                   Props1 = add_global_name(Proc, Props),
                   Props2 = [{pid, pid_to_list(Proc)} | size_props_to_bytes(Props1)],
                   [Props2 | Acc]
           end,
    process_info2(Tail, Acc1).

add_global_name(Proc, Props) ->
    case find_global_name(Proc) of
        not_found ->
            Props;
        Name ->
            [{global_name, Name} | Props]
    end.

find_global_name(Pid) ->
    Names = [Name || Name <- global:registered_names(), global:whereis_name(Name) =:= Pid],
    case Names of
        [] ->
            not_found;
        [Name] ->
            Name
    end.

size_props_to_bytes(Props) ->
    [size_to_bytes(KV) || KV <- Props].

size_to_bytes({heap_size = K, Size})       -> {K, Size * erlang:system_info(wordsize)};
size_to_bytes({stack_size = K, Size})      -> {K, Size * erlang:system_info(wordsize)};
size_to_bytes({total_heap_size = K, Size}) -> {K, Size * erlang:system_info(wordsize)};
size_to_bytes(KV)                          -> KV.

get_process_state(Pid_string) when is_list(Pid_string) ->
    Pid = list_to_pid(Pid_string),
    State = sys:get_state(Pid, 3000),
    case proc_lib:initial_call(Pid) of
        {Mod,_,_} ->
            {ok, format_record(State, Mod)};
        false ->
            {ok, State}
    end.

%%
format_record(Rec, Mod) when is_list(Rec) ->
    [format_record(R, Mod) || R <- Rec];
format_record(Rec, Mod) when is_atom(element(1, Rec)) ->
    try
        File = code:which(Mod),
        {ok,{_Mod,[{abstract_code,{_Version,Forms}},{"CInf",_CB}]}} = beam_lib:chunks(File, [abstract_code,"CInf"]),
        [Name | RecValues] = tuple_to_list(Rec),
        [FieldNames] = [record_fields(Fields) || {attribute,_,record,{Tag,Fields}} <- Forms, Tag =:= Name],
        FieldsAsTuples = lists:zipwith(
                           fun(K, V) -> {erlyberly_record_field, K, V} end,
                           FieldNames,
                           [format_record(R, Mod) || R <- RecValues]),
        {erlyberly_record, Name, FieldsAsTuples}
    catch
        _:_ ->
            %% The module might not contain abstract_code (compiled without
            %% debug_info) or trying to treat a normal tuple as record.
            Rec
    end;
format_record(Other, _Mod) ->
    Other.

record_fields([{record_field,_,{atom,_,Field}} | Fs]) ->
    [Field | record_fields(Fs)];
record_fields([{record_field,_,{atom,_,Field},_} | Fs]) ->
    [Field | record_fields(Fs)];
record_fields([]) ->
    [].

%%% ============================================================================
%%% module function tree
%%% ============================================================================

module_functions() ->
    lists:sort(
        [module_functions2(Mod) || {Mod, _FPath} <- code:all_loaded()]
    ).

module_functions2(Mod) when is_atom(Mod) ->
    Exports = Mod:module_info(exports),
    Unexported = [F || F <- Mod:module_info(functions), not lists:member(F, Exports)],
    {Mod, Exports, Unexported}.


%%% ============================================================================
%%% tracing
%%% ============================================================================

%%
start_trace({Node, Pid}, Mod, Func, Arity, Max_queue_len) when is_atom(Node),
                                                               is_pid(Pid),
                                                               is_integer(Max_queue_len) ->
    ensure_dbg_started({Node, Pid}, Max_queue_len),
    Ref = make_ref(),
    erlyberly_tcollector ! {start_trace, Mod, Func, Arity, self(), Ref},
    receive
        {ok, Ref} ->
            {ok, whereis(erlyberly_tcollector)};
        Error when element(1,Error) == error ->
            Error
    after
        1000 ->
            {error, timeout}
    end.

%%
stop_trace(Mod, Func, Arity, _IsExported) ->
    erlyberly_tcollector ! {stop_trace, Mod, Func, Arity}.
%%
stop_traces() ->
    erlyberly_tcollector ! stop_traces.
%%
when_process_is_unregistered(ProcName, Fn) ->
    case whereis(ProcName) of
        undefined -> Fn();
        _         -> ok
    end.
%%
ensure_dbg_started({Eb_Node, Eb_pid}, Max_queue_len) ->
    % restart dbg
    when_process_is_unregistered(dbg, fun dbg:start/0),
    StartFn =
        fun() -> 
            Pid = proc_lib:spawn(?MODULE, erlyberly_tcollector, [Eb_Node, Eb_pid, Max_queue_len]),
            register(erlyberly_tcollector, Pid),
            % create a tracer that will send the trace logs to erlyberly_tcollector
            % to be stored.
            TraceFn = 
                fun (Trace, _) -> 
                    store_trace(Trace),
                    ok
                end,
            dbg:tracer(process, {TraceFn, ok})
        end,
    when_process_is_unregistered(erlyberly_tcollector, StartFn).

%%
store_trace(Trace) ->
    erlyberly_tcollector ! Trace.

-record(tcollector, {
    ui_pid,

    %%
    traces = [],

    %% the maximum message queue length that the collector can accrue before
    %% tracing is suspended
    max_queue_len
}).

erlyberly_tcollector(Node, Pid, Max_queue_len) when is_integer(Max_queue_len) ->
    % throws a badarg if the node has already closed down
    erlang:monitor_node(Node, true),

    % apply a trace on the returns of the code module, so we can listen for 
    % code reloads, a code reload removes all traces on that module so when we
    % receive this message, reapply all traces for that module
    {ok,_} = dbg:p(all, [c, timestamp]),
    {ok,_} = dbg:tp(code, x),
    erlyberly_tcollector2(#tcollector{ ui_pid = Pid,
                                       max_queue_len = Max_queue_len }).

%%
erlyberly_tcollector2(#tcollector{ ui_pid = UI_pid, max_queue_len = Max_queue_len } = TC) ->
    case process_info(self(), message_queue_len) of
        {message_queue_len, Queue_len} when Queue_len > Max_queue_len ->
            %% io:format("~nSTOPPING TRACING, Queue Len: ~p, Max Len: ~p~n", [Queue_len, Max_queue_len]),
            ok = dbg:stop_clear(),
            UI_pid ! {erlyberly_trace_overload, Queue_len},
            collect_overloading_logs(Max_queue_len, TC);
        _ ->
            receive_next_trace(TC)
    end.

%% Collect logs that in the message queue, even though we have suspended
%% tracing. This is to try and show what was causing the overload.
collect_overloading_logs(0, _) ->
    ok;
collect_overloading_logs(Max_queue_len, TC) ->
    receive
        Log ->
            case collect_log(Log) of
                skip ->
                    collect_overloading_logs(Max_queue_len-1, TC);
                {erlyberly_module_loaded, _} ->
                    collect_overloading_logs(Max_queue_len-1, TC);
                Trace_log ->
                    notify_erlyberly_trace_log(Trace_log, TC),
                    collect_overloading_logs(Max_queue_len-1, TC)
            end
    after
        0 -> ok
    end.

receive_next_trace(#tcollector{ traces = Traces } = TC) ->
    receive
        {start_trace, Mod, Func, Arity, Requester, Ref} ->
            TC1 = tcollector_start_trace(Mod, Func, Arity, TC),
            Requester ! {ok, Ref},
            erlyberly_tcollector2(TC1);
        {stop_trace, Mod, Func, Arity} ->
            dbg:ctpl(Mod, Func, Arity),
            Traces_1 = Traces -- [{Mod, Func, Arity}],
            TC1 = TC#tcollector{ traces = Traces_1 },
            erlyberly_tcollector2(TC1);
        stop_traces ->
            ok = dbg:stop_clear();
        {nodedown, _Node} ->
            ok = dbg:stop_clear();
        Log ->
            case collect_log(Log) of
                skip ->
                    ok;
                {erlyberly_module_loaded, Loaded_module} ->
                    ok = reapply_traces(Loaded_module, TC#tcollector.traces),
                    ok = notify_erlyberly_module_loaded(Loaded_module, TC);
                Trace_log ->
                    notify_erlyberly_trace_log(Trace_log, TC)
            end,
            erlyberly_tcollector2(TC)
   end.

%%
tcollector_start_trace(Mod, Func, Arity, #tcollector{ traces = Traces } = TC) ->
    Match_spec = [{'_', [], [{message,{process_dump}}, {exception_trace}]}],
    Trace_spec = {Mod, Func, Arity},
    dbg:tpl(Trace_spec, Match_spec),
    dbg:p(all, [c, timestamp]),
    TC#tcollector{ traces = [Trace_spec | Traces] }.

%%
collect_log({trace_ts, _, return_from, {code, ensure_loaded, _}, _}) ->
    % ensure loaded can be called many times for one reload so just skip it
    skip;
collect_log({trace_ts, _, return_from, {code, _, _}, {module, Loaded_module}, _}) ->
    % if we trace that a module is reloaded then reapply traces to it
    %ok = reapply_traces(Loaded_module, TC#tcollector.traces),
    %ok = notify_erlyberly_module_loaded(Loaded_module, TC),
    {erlyberly_module_loaded, Loaded_module};
collect_log({trace_ts, _, _, {code, _, _}, _}) ->
    skip;
collect_log({trace_ts, _, _, {code, _, _}}) ->
    skip;
collect_log(Trace) when element(1, Trace) == trace_ts orelse element(1, Trace) == trace ->
    trace_to_props(Trace);
collect_log(U) ->
    io:format("unknown trace ~p~n", [U]),
    skip.

notify_erlyberly_module_loaded(Loaded_module, #tcollector{ ui_pid = Pid }) ->
    Pid ! {erlyberly_module_loaded, Loaded_module, module_functions2(Loaded_module)},
    ok.

notify_erlyberly_trace_log(Trace_log, #tcollector{ ui_pid = Pid }) ->
    Pid ! {erlyberly_trace_log, Trace_log},
    ok.

%% Convert a 'call' trace (a normal function call, before return) to a
%% {call, proplists()} that erlyberly can understand.
call_trace_props(Pid, Func, Timestamp, Extra_props)  ->
    {call, 
        [ {pid, pid_to_list(Pid)},
          {reg_name, get_registered_name(Pid)},
          {fn, Func},
          {timetamp_call_us, timestamp_to_us(Timestamp)} | Extra_props ]}.

%% Covert a 'return_from' trace into proplists erlyberly can understand
return_from_trace_props(Pid, Result, Timestamp) ->
    {return_from,
        [ {pid, pid_to_list(Pid)},
          {reg_name, get_registered_name(Pid)},
          {result, Result},
          {timetamp_return_us, timestamp_to_us(Timestamp)} ]}.

%%
trace_to_props({trace_ts, Pid, call, {Mod, Func_name, [Req, State]}, _, Timestamp}) when Func_name == handle_info orelse
                                                                                         Func_name == handle_cast ->
    call_trace_props(Pid, {Mod, Func_name, [Req, format_record(State, Mod)]}, Timestamp, []);
trace_to_props({trace_ts, Pid, call, {Mod, handle_call, [Req, From, State]}, _, Timestamp}) ->
    call_trace_props(Pid, {Mod, handle_call, [Req, From, format_record(State, Mod)]}, Timestamp, []);
trace_to_props({trace_ts, Pid, call, Func, Msg, Timestamp}) ->
    Stack_trace =
        try
            [{stack_trace, stak(Msg)}]
        catch
            _C:_Error ->
                % io:format("ERROR ~p~n", [_Error])
                []
        end,
    % call handler for everything else
    call_trace_props(Pid, Func, Timestamp, Stack_trace);
trace_to_props({trace_ts, Pid, return_from, {Mod, Func_name, _}, {noreply, Rec}, Timestamp}) when Func_name == handle_info orelse
                                                                                                  Func_name == handle_cast orelse
                                                                                                  Func_name == handle_call ->
    return_from_trace_props(Pid, {noreply, format_record(Rec, Mod)}, Timestamp);

trace_to_props({trace_ts, Pid, return_from, {Mod, handle_call, _}, {reply, Reply, Rec}, Timestamp}) ->
    return_from_trace_props(Pid, {reply, Reply, format_record(Rec, Mod)}, Timestamp);
trace_to_props({trace_ts, Pid, return_from, _Func, Result, Timestamp}) ->
    % return from handler for everything else
    return_from_trace_props(Pid, Result, Timestamp);
trace_to_props({trace_ts, Pid, exception_from, Func, {Class, Value}, Timestamp}) ->
    {exception_from,
        [ {pid, pid_to_list(Pid)},
          {reg_name, get_registered_name(Pid)},
          {fn, Func},
          {exception_from, {Class, Value}},
          {timetamp_return_us, timestamp_to_us(Timestamp)} ]};
trace_to_props({trace, Pid, call, Func, _}) ->
    {call,
        [ {pid, pid_to_list(Pid)},
          {reg_name, get_registered_name(Pid)},
          {fn, Func} ]};
trace_to_props({trace, Pid, exception_from, Func, {Class, Value}}) ->
    {exception_from, 
        [ {pid, pid_to_list(Pid)},
          {reg_name, get_registered_name(Pid)},
          {fn, Func},
          {exception_from, {Class, Value}} ]};
trace_to_props({trace, Pid, return_from, Func, Result}) ->
    {return_from, 
        [ {pid, pid_to_list(Pid)},
          {reg_name, get_registered_name(Pid)},
          {fn, Func},
          {result, Result} ]};
trace_to_props(U) ->
    io:format("skipped trace_ts ~p~n", [U]),
    skip.                     

%%
reapply_traces(Loaded_module, Traces) ->
    % filter out the traces for the reloaded, module, could be
    % done in the list comp but it causes a compiler warning
    Traces_1 = lists:filter(fun(T) -> 
                                element(1, T) == Loaded_module 
                            end, Traces),

    % reapply each trace that has the loaded module
    [erlyberly_tcollector ! {start_trace, M, F, A} || {M, F, A} <- Traces_1],
    ok.
%%
collect_trace_logs() ->
    case whereis(erlyberly_tcollector) of
        undefined ->
            % monitoring of a pid from jinterface is not implemented as far as I can
            % tell so just make do with polling
            {error, tcollector_down};
        _ ->
            erlyberly_tcollector ! {take_logs, self()},
            receive
                {trace_logs, Logs} -> {ok, Logs}
            after 2000 -> {error, tcollector_timeout}
            end
    end.
%%
get_registered_name(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        [{_, Name}] -> Name;
        {_, Name}   -> Name;
        _           -> undefined
    end.

timestamp_to_us({Mega, Sec, Micros}) ->
    (((Mega * 1000000) + Sec) * 1000000) + Micros.

%%% =============================================================================
%%%
%%% seq_trace
%%%
%%% this could be its own module, but it is handy to keep everything in a
%%% single module so that code injection on the remote node is simple.
%%%
%%% =============================================================================

-define(erlyberly_seq_trace, erlyberly_seq_trace).


%%
seq_trace(Node_pid, Mod, Function_name, Arity, Is_exported) ->
    % TODO monitor the pid
    {ok, _Seq_trace_pid} = ensure_seq_tracer_started(Node_pid),

    case Is_exported of
        true ->
            dbg:tp(Mod, Function_name, seq_trace_match_spec(Arity));
        false ->
            dbg:tpl(Mod, Function_name, seq_trace_match_spec(Arity))
    end,
    dbg:p(all, c),
    ok.

%%
collect_seq_trace_logs() ->
    case whereis(?erlyberly_seq_trace) of
        undefined ->
            % monitoring of a pid from jinterface is not implemented as far as I can
            % tell so just make do with polling
            {error, down};
        _ ->
            ?erlyberly_seq_trace ! {take_seq_logs, self()},
            receive
                {seq_trace_logs, Logs} -> 
                    {ok, Logs}
            after 2000 ->
                {error, timeout}
            end
    end.

%%
ensure_seq_tracer_started(Remote_node) ->
    %% TODO configure value for seq trace max queeu length
    ensure_dbg_started(Remote_node, 500),

    case whereis(?erlyberly_seq_trace) of
        undefined ->
            start_seq_tracer(Remote_node);
        Pid ->
            {ok, Pid}
    end.

%%
start_seq_tracer({Node, _}) ->
    Tracer_collector_pid = 
        proc_lib:spawn(
            fun() ->
                % throws a badarg if the node has already closed down
                erlang:monitor_node(Node, true),

                % start seq trace
                seq_trace:set_system_tracer(self()),

                seq_trace_collector([])
            end),
    register(?erlyberly_seq_trace, Tracer_collector_pid),
    {ok, Tracer_collector_pid}.

%%
seq_trace_collector(Trace_logs) ->
    receive
        {nodedown, _Node} ->
            ok = dbg:stop_clear(),
            true = seq_trace:reset_trace();
        {seq_trace, _Label, Trace_log, Timestamp} ->
            Trace_props = seq_trace_to_props(Trace_log, Timestamp),
            seq_trace_collector([Trace_props | Trace_logs]);
        {take_seq_logs, Pid} ->
            Pid ! {seq_trace_logs, lists:reverse(Trace_logs)},
            seq_trace_collector([]);
        Other ->
            io:format("Erlyberly seq_trace, unexpected message: ~s ~p~n", [format_utc_timestamp(), Other]),

            seq_trace_collector(Trace_logs)
    end.

%%
seq_trace_to_props({Msg_type, Serial, From, To, Message}, Timestamp) ->
    [ {msg_type, Msg_type},
      {serial, Serial},
      {from, format_pid(From)},
      {to, format_pid(To)},
      {message, Message},
      {timestamp, Timestamp} ].

%%
format_pid(Pid) when is_pid(Pid) ->
    Proc_info = (catch process_info(Pid, registered_name)),
    case Proc_info of
        {registered_name, Reg_name} when is_atom(Reg_name) ->
            atom_to_list(Reg_name);
        _ ->
            pid_to_list(Pid)
    end;
format_pid(Reg_name) when is_atom(Reg_name) ->
    atom_to_list(Reg_name);
format_pid(Port) when is_port(Port) ->
    erlang:port_to_list(Port).

%%
format_utc_timestamp() ->
    TS = {_,_,Micro} = os:timestamp(),
    {{Year,Month,Day},{Hour,Minute,Second}} = 
    calendar:now_to_universal_time(TS),
    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul",
              "Aug","Sep","Oct","Nov","Dec"}),
    io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w",
          [Day,Mstr,Year,Hour,Minute,Second,Micro]).

-define(SET_SEQ_TOKEN, 
            set_seq_token(send, true),
            set_seq_token('receive', true),
            set_seq_token(timestamp, true),
            set_seq_token(print, true)).

-include_lib("stdlib/include/ms_transform.hrl").

%%
seq_trace_match_spec(0) ->
    dbg:fun2ms(fun(_) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(1) ->
    dbg:fun2ms(fun([_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(2) ->
    dbg:fun2ms(fun([_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(3) ->
    dbg:fun2ms(fun([_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(4) ->
    dbg:fun2ms(fun([_,_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(5) ->
    dbg:fun2ms(fun([_,_,_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(6) ->
    dbg:fun2ms(fun([_,_,_,_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(7) ->
    dbg:fun2ms(fun([_,_,_,_,_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(8) ->
    dbg:fun2ms(fun([_,_,_,_,_,_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(9) ->
    dbg:fun2ms(fun([_,_,_,_,_,_,_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(10) ->
    dbg:fun2ms(fun([_,_,_,_,_,_,_,_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(11) ->
    dbg:fun2ms(fun([_,_,_,_,_,_,_,_,_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(12) ->
    dbg:fun2ms(fun([_,_,_,_,_,_,_,_,_,_,_,_]) -> ?SET_SEQ_TOKEN end);
seq_trace_match_spec(_) ->
    error(arity_too_large).

%%% ============================================================================
%%% xref analysis
%%% ============================================================================

-define(erlyberly_xref, erlyberly_xref).

%%
xref_analysis(Ignore_mods, M,F,A) when is_integer(A) ->
    Call_stack_set = gb_sets:new(),
    xref_analysis2({M,F,A}, gb_sets:from_list(Ignore_mods), Call_stack_set, []).

%%
xref_analysis2(MFA, Ignore_mods_set, All_calls, Call_stack) ->
    case gb_sets:is_member(MFA, All_calls) orelse gb_sets:is_member(element(1,MFA), Ignore_mods_set) of
        true ->
            {MFA, []};
        false ->
            {ok, Calls} = xref:analyze(?erlyberly_xref, {call, MFA}),
            All_calls2 = gb_sets:add(MFA, All_calls),
            Analysed_calls =
                [xref_analysis2(X_mfa, Ignore_mods_set, All_calls2, [MFA | Call_stack]) || X_mfa <- Calls, not is_xref_recursion(MFA, X_mfa)],
            {MFA, Analysed_calls}
    end.

%%
is_xref_recursion({M,F,A}, {M,F,A}) ->
    true;
is_xref_recursion(_,_) ->
    false.

%%
ensure_xref_started() ->
    catch xref:stop(?erlyberly_xref),
    {ok, _} = xref:start(?erlyberly_xref),
    Excluded = ["asn1ct", "/ct-", "dialyzer", "diameter", "hipe", "httpd", "megaco", "xmerl", "wx-"],
    [xref:add_directory(?erlyberly_xref, Dir) || Dir <- code:get_path(), not dir_contains(Dir, Excluded)],
    {erlyberly_xref_started}.

dir_contains(_, []) ->
    false;
dir_contains(Dir, [Str|Tail]) ->
    case string:str(Dir, Str) of
        0 -> dir_contains(Dir, Tail);
        _ -> true
    end.

%%% ============================================================================
%%% view module source code
%%% ============================================================================

-spec get_source_code(module() | {module(), function(), Arity :: non_neg_integer()}) -> {ok, string()} | {error, Reason :: any()}.
get_source_code(What) ->
    try
        case What of
            {M,F,A}               -> fun_src(M,F,A);
            Mod when is_atom(Mod) -> mod_src(Mod)
        end
    catch
        _:E ->
            {error, list_to_binary(
                lists:flatten(io_lib:format("E:~p stack:~p", [E,erlang:get_stacktrace()]))
            )}
    end.

-spec get_abstract_code(module() | {module(), function(), Arity :: non_neg_integer()}) -> {ok,[erl_parse:abstract_form()]} | {ok, string()} | {error, Reason :: any()}.
get_abstract_code(What) ->
    try
        case What of
            {M,F,A}               -> fun_abst(M,F,A);
            Mod when is_atom(Mod) -> mod_abst(Mod)
        end
    catch
        _:_E ->
            {error, list_to_binary(
                lists:flatten(io_lib:format("~p", [erlang:get_stacktrace()]))
            )}
    end.

mod_src(Module) ->
    abstract_code(Module, fun(Forms) -> 
        {ok, list_to_binary(
            lists:flatten([[erl_pp:form(F),$\n] || F <- Forms, element(1,F) =:= attribute orelse element(1,F) =:= function])
        )}
    end).
 
fun_src(Mod, Fun, Arity) ->
    abstract_code(Mod, fun(Forms) -> 
        [FF] = [FF || FF = {function, _Line, Fun2, Arity2, _} <- Forms, Fun2 =:= Fun, Arity2 =:= Arity],
        {ok, list_to_binary(
            lists:flatten(erl_pp:form(FF))
        )}
    end).

mod_abst(Module) ->
    abstract_code(Module, fun(Forms) -> 
        {ok, list_to_binary(
            lists:flatten(io_lib:format("~p", [Forms]))
        )}
    end).

fun_abst(Mod, Fun, Arity) ->
    abstract_code(Mod, fun(Forms) -> 
        [FF] = [FF || FF = {function, _Line, Fun2, Arity2, _} <- Forms, Fun2 =:= Fun, Arity2 =:= Arity],
        {ok, list_to_binary(
            lists:flatten(io_lib:format("~p", [FF]))
        )}
    end).

abstract_code(Module, ExecFun) ->
        File = code:which(Module),
        case beam_lib:chunks(File, [abstract_code]) of 
            {ok,{_Mod,[{abstract_code,no_abstract_code}]}} ->
                {ok,"no_abstract_code for Module "+atom_to_list(Module)+"."};
            {ok,{_Mod,[{abstract_code,{_Version,Forms}}]}} ->
                ExecFun(Forms)
        end.


%%% ============================================================================
%%% error_logger gen_event handler
%%% ============================================================================


-record(err_state, { node }).

init([Node]) ->
    {ok, #err_state{ node = Node}}.

handle_event({error_report,_,{_, crash_report, Crash_props_1}}, State) ->
    Node = (State#err_state.node),
    %{value, {error_info,}, Crash_props_2} = lists:keytake(error_info, 1, lists:flatten(Crash_props_1)),
    Node ! {erlyberly_error_report, lists:flatten(Crash_props_1)},
    {ok, State};
handle_event(_, State) ->
    % io:format("error: ~p ~p~n", [element(1,E), tuple_size(E) ]), 

    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% ============================================================================
%%% tracing `process_dump` parser from redbug
%%% ============================================================================

%%% https://github.com/massemanet/eper/blob/f7a1b4504f5eefc61fb9da7101fdaccc687021cd/src/redbug.erl
%%%
%%% Copyright (c) 2008-2013 mats cronqvist
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.

stak(Bin) ->
  lists:foldr(fun munge/2,[],string:tokens(binary_to_list(Bin),"\n")).

munge(I,Out) ->
  case lists:reverse(I) of
    "..."++_ -> [truncated|Out];
    _ ->
      case string:str(I, "Return addr") of
        0 ->
          case string:str(I, "CP:") of
            0 -> Out;
            _ -> [mfaf(I)|Out]
          end;
        _ ->
          case string:str(I, "erminate process normal") of
            0 -> [mfaf(I)|Out];
            _ -> Out
          end
      end
  end.

mfaf(I) ->
  [_, C|_] = string:tokens(I,"()+"),
  stack_to_mfa(C).

stack_to_mfa(String) ->
    case string:tokens(String, ":/") of
        [Mod, Func, Arity] ->
            {list_to_existing_atom(fix_elixir_strings(Mod)),
             list_to_existing_atom(fix_elixir_strings(Func)),
             list_to_integer(string:strip(fix_elixir_strings(Arity)))};
        _ ->
            String
    end.

fix_elixir_strings(String) ->
    re:replace(String, "\"|\'", "", [global, {return, list}]).

%%% ============================================================================
%%% erlang fun decompiler
%%% ============================================================================

%%% Copyright (c) 2009 Serge Aleynikov
%%%
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without restriction,
%%% including without limitation the rights to use, copy, modify, merge,
%%% publish, distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do
%%% so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.

%% @doc Decompile a function to its source text
saleyn_fun_src(Fun) when is_function(Fun) ->
    saleyn_fun_src(Fun, []).

%% @doc Decompile a function to its source text
-spec saleyn_fun_src(function(), Options :: [verbose | ast]) -> string().
saleyn_fun_src(Fun, Options) when is_function(Fun), is_list(Options) ->
    {module, Mod} = erlang:fun_info(Fun, module),
    {name, Name}  = erlang:fun_info(Fun, name),
    {ok, Module, _Beam, Forms} = saleyn_get_abstract_code(Mod),
    {F, Arity, Pos} = fun_name(Name),
    saleyn_fun_src(Module, Name, F, Arity, Pos, Forms, Fun, Options).

saleyn_fun_src(erl_eval, _Name, expr, _Arity, _Pos, _Forms, Fun, Options) ->
    case erlang:fun_info(Fun, env) of
        {env, [_, _, _, Abst | _]} ->
            ok;
        {env,[{[], _, _, Abst}]} ->
            ok
    end,
    Ast = erl_syntax:form_list(Abst),
    saleyn_fun_src2(format, Ast, Options);
saleyn_fun_src(_Module, _Name, F, Arity, Pos, Forms, _Fun, Options) ->
    Clauses = [Cs || {function, _, Fun, A, Cs} <- Forms, Fun == F, A == Arity],
    Funs    = funs(lists:concat(Clauses)),
    Ast     = lists:nth(Pos, Funs),
    saleyn_fun_src2(undefined, Ast, Options).

saleyn_fun_src2(Envelope, Ast, _Options) ->
    Text = erl_prettypr:format(Ast),
    case Envelope of
        format ->
            "fun " ++ Text ++ " end.";
        _ ->
            Text ++ "."
    end.

fun_name(Name) ->
    [Fs, As, _, Rs] = string:tokens(atom_to_list(Name), "-/"),
    {list_to_atom(Fs), list_to_integer(As), list_to_integer(Rs)+1}.

funs(L) ->
    lists:reverse(lists:foldl(fun
        ({'fun',_,_} = F, A)    -> [F | A];
        (T, A) when is_tuple(T) -> funs(lists:flatten(tuple_to_list(T))) ++ A;
        (_, A)                  -> A
    end, [],  L)).

saleyn_get_abstract_code(Module) when is_atom(Module) ->
    {module,_} = code:ensure_loaded(Module),
    Beam = code:which(Module),
    saleyn_get_abstract_code(Beam);
saleyn_get_abstract_code(Beam) when is_list(Beam) ->
    Basename = filename:basename(Beam, ".beam"),
    case beam_lib:chunks(Beam, [abstract_code]) of
    {ok, {Module,[{abstract_code,{_,AC}}]}} ->
       {ok, Module, Basename, AC};
    Other ->
       Other
    end.
