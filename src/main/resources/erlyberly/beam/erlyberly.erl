
-module(erlyberly).

-export([process_info/0]).

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