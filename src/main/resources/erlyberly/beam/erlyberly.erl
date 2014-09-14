
-module(erlyberly).

-export([ collect_trace_logs/0,
          erlyberly_tcollector/0,
          module_functions/0,
          process_info/0,
          start_trace/4 ]).

%% code liberally stolen from entop
process_info() ->
    [[{pid, pid_to_list(P)} | process_info_items(P)] || P <- erlang:processes()].

process_info_items(P) ->
    erlang:process_info(P, [registered_name,
                            reductions,
                            message_queue_len,
                            heap_size,
                            stack_size,
                            total_heap_size]).

module_functions() ->
    [module_functions2(Mod) || {Mod, _FPath} <- code:all_loaded()].

module_functions2(Mod) when is_atom(Mod) ->
    Exports = Mod:module_info(exports),
    Unexported = [F || F <- Mod:module_info(functions), not lists:member(F, Exports)],
    {Mod, Exports, Unexported}.

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

erlyberly_tcollector() ->
    erlyberly_tcollector2([]).

erlyberly_tcollector2(Acc) ->
    receive
        {take_logs, Pid} ->
            Pid ! {trace_logs, lists:reverse(Acc)},
            erlyberly_tcollector2([]);
        Log ->
            erlyberly_tcollector2([Log | Acc])
    end.

collect_trace_logs() ->
    erlyberly_tcollector ! {take_logs, self()},
    receive
        {trace_logs, Logs} -> Logs
    after 2000 -> fail
    end.