-module(util).

-export([atomic_controlling_process_spawn/3]).

% Start atomic_controlling_process_spawn
controlling_process_list(_, []) ->
  [];
controlling_process_list(Pid, [Socket | List]) ->
  [ gen_tcp:controlling_process(Socket, Pid) | controlling_process_list(Pid, List) ].

stop_go(Function, Arg) ->
  receive
    go ->
      Function(Arg)
  end.

atomic_controlling_process_spawn(Function, Arglist, Socketlist) ->
  Pid = spawn(fun() -> stop_go(Function, Arglist) end),
  Result = controlling_process_list(Pid, Socketlist),
  Pid ! go,
  { Pid , Result }.
% End atomic_controlling_process_spawn
