
-module(erlyberly).

-export([ module_functions/0,
          process_info/0 ]).

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