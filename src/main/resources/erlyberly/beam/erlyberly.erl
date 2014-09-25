
-module(erlyberly).

-export([ collect_trace_logs/0,
          erlyberly_tcollector/0,
          module_functions/0,
          process_info/0,
          start_trace/4 ]).

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

start_trace(Mod, Func, Arity, IsExported) ->
    % start the process that will collect the traces
    case whereis(erlyberly_tcollector) of
        undefined -> 
            Pid = spawn(?MODULE, erlyberly_tcollector, []),
            register(erlyberly_tcollector, Pid);
        _ -> 
            ok
    end,

    % restart dbg
    dbg:stop_clear(),
    dbg:start(),

    % create a tracer that will send the trace logs to erlyberly_tcollector
    % to be stored.
    TraceFn = fun (Trace, _) -> 
                  store_trace(Trace),
                  ok
              end,
    dbg:tracer(process, {TraceFn, ok}),

    case IsExported of
        true  -> dbg:tp(Mod, Func, Arity, cx);
        false -> dbg:tpl(Mod, Func, Arity, cx)
    end,
    dbg:p(all, c).


store_trace(Trace) ->
    erlyberly_tcollector ! Trace.

-record(tcollector, { call, logs = [] }).

erlyberly_tcollector() ->
    erlyberly_tcollector2(#tcollector{}).

erlyberly_tcollector2(#tcollector{ logs = Logs } = TC) ->
    receive
        {take_logs, Pid} ->
            Pid ! {trace_logs, lists:reverse(Logs)},
            erlyberly_tcollector2(TC#tcollector{ logs = []});
        Log ->
            TC1 = collect_log(Log, TC),
            erlyberly_tcollector2(TC1)
    end.

collect_log({trace, Pid, call, Args, CallingMFA}, #tcollector{ call = undefined } = TC) ->
    TC#tcollector{ call = {Pid, Args, CallingMFA} };
collect_log({trace, Pid, return_from, _Func, Result}, #tcollector{ call = {Pid, Args, CallingMFA}, logs = Logs } = TC) ->
    
    Reg_name = case erlang:process_info(Pid, registered_name) of
                   [{_, Name}] -> Name;
                   {_, Name} -> Name;
                   _ -> undefined
               end,
    Log = [ {pid, pid_to_list(Pid)},
            {reg_name, Reg_name},
            {calling_fn, CallingMFA},
            {fn, Args},
            {result, Result} ],
    TC#tcollector{ call = undefined, logs = [Log | Logs]};
collect_log(_, TC) ->
    TC.


collect_trace_logs() ->
    erlyberly_tcollector ! {take_logs, self()},
    receive
        {trace_logs, Logs} -> Logs
    after 2000 -> fail
    end.