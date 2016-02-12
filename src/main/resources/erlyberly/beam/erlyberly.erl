
-module(erlyberly).

-export([collect_seq_trace_logs/0]).
-export([collect_trace_logs/0]).
-export([ensure_xref_started/0]).
-export([erlyberly_tcollector/1]).
-export([get_abstract_code/1]).
-export([get_process_state/1]).
-export([get_source_code/1]).
-export([module_functions/0]).
-export([process_info/0]).
-export([seq_trace/5]).
-export([start_trace/5]).
-export([stop_traces/0]).
-export([stop_trace/4]).
-export([xref_analysis/3]).

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
                   Props1 = [{pid, pid_to_list(Proc)} | size_props_to_bytes(Props)],
                   [Props1 | Acc]
           end,
    process_info2(Tail, Acc1).

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
    [module_functions2(Mod) || {Mod, _FPath} <- code:all_loaded()].

module_functions2(Mod) when is_atom(Mod) ->
    Exports = Mod:module_info(exports),
    Unexported = [F || F <- Mod:module_info(functions), not lists:member(F, Exports)],
    {Mod, Exports, Unexported}.


%%% ============================================================================
%%% tracing
%%% ============================================================================

%%
start_trace({Node, Pid}, Mod, Func, Arity, _IsExported) ->
    ensure_dbg_started({Node, Pid}),

    erlyberly_tcollector ! {start_trace, Mod, Func, Arity},

    {ok, whereis(erlyberly_tcollector)}.
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
ensure_dbg_started({Eb_Node, _}) ->
    % restart dbg
    when_process_is_unregistered(dbg, fun dbg:start/0),

    StartFn = 
        fun() -> 
            Pid = spawn(?MODULE, erlyberly_tcollector, [Eb_Node]),
            register(erlyberly_tcollector, Pid)
        end,

    when_process_is_unregistered(erlyberly_tcollector, StartFn),

    % create a tracer that will send the trace logs to erlyberly_tcollector
    % to be stored.
    TraceFn = 
        fun (Trace, _) -> 
            store_trace(Trace),
            ok
        end,
    dbg:tracer(process, {TraceFn, ok}).

%%
store_trace(Trace) ->
    erlyberly_tcollector ! Trace.

-record(tcollector, {
    %%
    logs = [],

    %%
    traces = []
}).

erlyberly_tcollector(Node) ->
    % throws a badarg if the node has already closed down
    erlang:monitor_node(Node, true),

    % apply a trace on the returns of the code module, so we can listen for 
    % code reloads, a code reload removes all traces on that module so when we
    % receive this message, reapply all traces for that module
    dbg:p(all, [c, timestamp]),
    dbg:tp(code, x),

    erlyberly_tcollector2(#tcollector{}).
%%
erlyberly_tcollector2(#tcollector{ logs = Logs, traces = Traces } = TC) ->
    receive
        {start_trace, _, _, _} = Eb_spec ->
            TC1 = tcollector_start_trace(Eb_spec, TC),
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
        {take_logs, Pid} ->
            Pid ! {trace_logs, lists:reverse(Logs)},
            erlyberly_tcollector2(TC#tcollector{ logs = []});
        Log ->
            TC1 = collect_log(Log, TC),
            erlyberly_tcollector2(TC1)
   end.
%%
tcollector_start_trace({start_trace, Mod, Func, Arity}, #tcollector{ traces = Traces } = TC) ->
    dbg:tpl(Mod, Func, Arity, cx),
    dbg:p(all, [c, timestamp]),
    Trace_spec = {Mod, Func, Arity},
    TC#tcollector{ traces = [Trace_spec | Traces] }.

%%
collect_log({trace_ts, _, return_from, {code, ensure_loaded, _}, _}, TC) ->
    % ensure loaded can be called many times for one reload so just skip it
    TC;
collect_log({trace_ts, _, return_from, {code, _, _}, {module, Loaded_module}, _}, TC) ->
    % if we trace that a module is reloaded then reapply traces to it
    ok = reapply_traces(Loaded_module, TC#tcollector.traces),
    TC;
collect_log({trace_ts, _, _, {code, _, _}, _}, TC) ->
    TC;
collect_log({trace_ts, _, _, {code, _, _}}, TC) ->
    TC;
collect_log(Trace, #tcollector{ logs = Logs } = TC) when element(1, Trace) == trace_ts orelse element(1, Trace) == trace ->
    Logs_1 = maybe_add_log(trace_to_props(Trace), Logs),
    TC#tcollector{ logs = Logs_1 };
collect_log(U, TC) ->
    io:format("unknown trace ~p~n", [U]),
    TC.

%%
maybe_add_log(skip, Logs) -> Logs;
maybe_add_log(Log, Logs)  -> [Log | Logs].

%% Convert a 'call' trace (a normal function call, before return) to a
%% {call, proplists()} that erlyberly can understand.
call_trace_props(Pid, Func, Timestamp)  ->
    {call, 
        [ {pid, pid_to_list(Pid)},
          {reg_name, get_registered_name(Pid)},
          {fn, Func},
          {timetamp_call_us, timestamp_to_us(Timestamp)} ]}.

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
    call_trace_props(Pid, {Mod, Func_name, [Req, format_record(State, Mod)]}, Timestamp);
trace_to_props({trace_ts, Pid, call, {Mod, handle_call, [Req, From, State]}, _, Timestamp}) ->
    call_trace_props(Pid, {Mod, handle_call, [Req, From, format_record(State, Mod)]}, Timestamp);
trace_to_props({trace_ts, Pid, call, Func, _, Timestamp}) ->
    % call handler for everything else
    call_trace_props(Pid, Func, Timestamp);
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
    ensure_dbg_started(Remote_node),

    case whereis(?erlyberly_seq_trace) of
        undefined ->
            start_seq_tracer(Remote_node);
        Pid ->
            {ok, Pid}
    end.

%%
start_seq_tracer({Node, _}) ->
    Tracer_collector_pid = 
        spawn(
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
xref_analysis(M,F,A) when is_integer(A) ->
    % TODO monitor the node and stop the erlyberly xref
    ensure_xref_started(),

    xref_analysis2({M,F,A}, []).

%%
xref_analysis2(MFA, Call_stack) ->
    {ok, Calls} = xref:analyze(?erlyberly_xref, {call, MFA}),
    {MFA, [xref_analysis2(X_mfa, [MFA | Call_stack]) || X_mfa <- Calls, not is_xref_recursion(MFA, X_mfa) andalso not lists:member(MFA, Call_stack)]}.

%%
is_xref_recursion({M,F,A}, {M,F,A}) ->
    true;
is_xref_recursion(_,_) ->
    false.

%%
ensure_xref_started() ->
    case whereis(?erlyberly_xref) of
        undefined ->
            {ok, _} = xref:start(?erlyberly_xref),
            % timer:sleep(1000),
            [xref:add_directory(?erlyberly_xref, Dir) || Dir <- code:get_path()],
            {erlyberly_xref_started};
        _ ->
            {erlyberly_xref_started}
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
