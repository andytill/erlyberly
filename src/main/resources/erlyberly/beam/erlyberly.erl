
-module(erlyberly).

-export([ collect_trace_logs/0,
          erlyberly_tcollector/1,
          module_functions/0,
          process_info/0,
          start_trace/5,
          stop_trace/4 ]).

%% ============================================================================
%% process info
%% ============================================================================

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

%% ============================================================================
%% module function tree
%% ============================================================================

module_functions() ->
    [module_functions2(Mod) || {Mod, _FPath} <- code:all_loaded()].

module_functions2(Mod) when is_atom(Mod) ->
    Exports = Mod:module_info(exports),
    Unexported = [F || F <- Mod:module_info(functions), not lists:member(F, Exports)],
    {Mod, Exports, Unexported}.


%% ============================================================================
%% tracing
%% ============================================================================

%%
start_trace({Node, Pid}, Mod, Func, Arity, IsExported) ->
    ensure_dbg_started({Node, Pid}),

    erlyberly_tcollector ! {start_trace, Mod, Func, Arity, IsExported},

    {ok, whereis(erlyberly_tcollector)}.
%%
stop_trace(Mod, Func, Arity, IsExported) ->
    erlyberly_tcollector ! {stop_trace, Mod, Func, Arity, IsExported}.
%%
when_process_unregistered(ProcName, Fn) ->
    case whereis(ProcName) of
        undefined -> Fn();
        _         -> ok
    end.
%%
ensure_dbg_started({Eb_Node, _Eb_Pid}) ->
    % restart dbg
    when_process_unregistered(dbg, fun dbg:start/0),

    StartFn = fun() -> 
                  Pid = spawn(?MODULE, erlyberly_tcollector, [Eb_Node]),
                  register(erlyberly_tcollector, Pid)
              end,

    when_process_unregistered(erlyberly_tcollector, StartFn),

    % create a tracer that will send the trace logs to erlyberly_tcollector
    % to be stored.
    TraceFn = fun (Trace, _) -> 
                  store_trace(Trace),
                  ok
              end,
    dbg:tracer(process, {TraceFn, ok}).

%%
store_trace(Trace) ->
    erlyberly_tcollector ! Trace.

-record(tcollector, {
    %% the last call, we need this to match up the result
    call, 

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
    dbg:tp(code, x),

    erlyberly_tcollector2(#tcollector{}).
%%
erlyberly_tcollector2(#tcollector{ logs = Logs, traces = Traces } = TC) ->
    receive
        {start_trace, _, _, _, _} = Eb_spec ->
            TC1 = tcollector_start_trace(Eb_spec, TC),
            erlyberly_tcollector2(TC1);
        {stop_trace, Mod, Func, Arity, IsExported} ->
            case IsExported of
                true  -> dbg:ctp(Mod, Func, Arity);
                false -> dbg:ctpl(Mod, Func, Arity)
            end,
            Traces_1 = Traces -- [{Mod, Func, Arity, IsExported}],
            TC1 = TC#tcollector{ traces = Traces_1 },
            erlyberly_tcollector2(TC1);
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
tcollector_start_trace({start_trace, Mod, Func, Arity, IsExported}, #tcollector{ traces = Traces } = TC) ->
    case IsExported of
        true  -> dbg:tp(Mod, Func, Arity, cx);
        false -> dbg:tpl(Mod, Func, Arity, cx)
    end,
    dbg:p(all, c),
    Trace_spec = {Mod, Func, Arity, IsExported},
    TC#tcollector{ traces = [Trace_spec | Traces] }.
%%
collect_log({trace, Pid, call, Args, CallingMFA}, #tcollector{ call = undefined } = TC) ->
    TC#tcollector{ call = {Pid, Args, CallingMFA} };
collect_log({trace, _, return_from, {code, ensure_loaded, _}, _}, TC) ->
    % ensure loaded can be called many times for one reload so just skip it
    TC;
collect_log({trace, _, return_from, {code, _, _}, {module, Loaded_module}}, TC) ->
    ok = reapply_traces(Loaded_module, TC#tcollector.traces),
    TC;
collect_log({trace, Pid, return_from, _Func, Result}, 
            #tcollector{ call = {Pid, Args, CallingMFA}, logs = Logs } = TC) ->
    Reg_name = get_registered_name(Pid),
    Log = [ {pid, pid_to_list(Pid)},
            {reg_name, Reg_name},
            {calling_fn, CallingMFA},
            {fn, Args},
            {result, Result} ],
    TC#tcollector{ call = undefined, logs = [Log | Logs]};
collect_log({trace, Pid, exception_from, _Func, {Class, Value}},
            #tcollector{ call = {Pid, Args, CallingMFA}, logs = Logs } = TC) ->
    Reg_name = get_registered_name(Pid),
    Log = [ {pid, pid_to_list(Pid)},
            {reg_name, Reg_name},
            {calling_fn, CallingMFA},
            {fn, Args},
            {exception_from, {Class, Value}} ],
    TC#tcollector{ call = undefined, logs = [Log | Logs]};
collect_log(_, TC) ->
    TC.
%%
reapply_traces(Loaded_module, Traces) ->
    % filter out the traces for the reloaded, module, could be
    % done in the list comp but it causes a compiler warning
    Traces_1 = lists:filter(fun(T) -> 
                                element(1, T) == Loaded_module 
                            end, Traces),

    % reapply each trace that has the loaded module
    [erlyberly_tcollector ! {start_trace, M, F, A, IsExported} || {M, F, A, IsExported} <- Traces_1],
    ok.
%%
collect_trace_logs() ->
    erlyberly_tcollector ! {take_logs, self()},
    receive
        {trace_logs, Logs} -> {ok, Logs}
    after 2000 -> fail
    end.
%%
get_registered_name(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        [{_, Name}] -> Name;
        {_, Name}   -> Name;
        _           -> undefined
    end.