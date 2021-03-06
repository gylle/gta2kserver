%%
%% gta2kserver
%%
%% Copyright Jonas Eriksson 2012
%%
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.
%%

-module(gta2k).

-export([start_server/0, hub_loop/1]).

-define(LISTEN_PORT, 9378).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, once}, inet6, inet]).

-record(user, {
    id,
    pid,
    user_data
  }).

-record(hub_state, {
    users = [],
    next_id = 0
  }).

start_server() ->
	case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
		{ok, LSocket} ->
      Hub = spawn(?MODULE, hub_loop, [#hub_state{}]),
      register(hub, Hub),
      io:format("Einar, GTA-klonserver~n", []),
      util:atomic_controlling_process_spawn(fun listen_loop/1, LSocket, [LSocket]);
		Error ->
			io:format("Error: ~p~n", [Error])
	end.

listen_loop(Listen) ->
  %inet:setopts(Listen, [{active, once}]),
  case gen_tcp:accept(Listen) of
    {ok, Socket} ->
      inet:setopts(Socket, ?TCP_OPTS),
      util:atomic_controlling_process_spawn(fun listen_loop/1, Listen, [Listen]),
      whereis(hub) ! { new_user, self() },
      client:new_client(Socket);
    {error, Reason} ->
			io:format("Accept falerade! ~p~n", [Reason])
  end.

% Start user list methods
% TODO: replace with more awesome data type
find_user([ #user{id = Id} = User | _ ], Id) ->
  User;
find_user([ _ | Users ], Id) ->
  find_user(Users, Id).

delete_user(Users, Id) ->
  DelUser = find_user(Users, Id),
  { lists:delete(DelUser, Users), DelUser }.

add_user(Users, NewUser) ->
  [ NewUser | Users ].
% End user list methods

hub_loop(State) ->
  receive
    { broadcast, Type, Sender, Data } ->
      % Send to everyone except Sender by filtering the user list
      AllPids = [ User#user.pid || User<-State#hub_state.users ],
      [ Pid ! { data_out, Type, Data } || Pid<-lists:filter(fun(Pid)-> not(Pid == Sender) end, AllPids) ],
      hub_loop(State);

    { new_user_id, Pid } ->
      % TODO: not safe for wraparound (protocol support up to 32 bit client id)
      Pid ! { new_user_id, State#hub_state.next_id },
      NewState = State#hub_state{next_id = State#hub_state.next_id + 1},
      hub_loop(NewState);

    { new_user, Pid, Id, UserData } ->
      % Create new users-list and new state
      NewUser = #user{id = Id, pid = Pid, user_data = UserData},
      NewUsers = add_user(State#hub_state.users, NewUser),
      NewState = State#hub_state{users = NewUsers},
      % Send ack to new user
      Pid ! { new_user_ack },
      % Send information to all existing users, which will inform the new user of their precense
      [ User#user.pid ! { new_user, Pid, UserData } || User<-State#hub_state.users ],
      hub_loop(NewState);

    { update_user_data, _Id, _UserData } ->
      hub_loop(State); % TODO

    { dropped_user, Id } ->
      { NewUsers, DroppedUser } = delete_user(State#hub_state.users, Id),
      [ User#user.pid ! { broadcast, dropped_user, DroppedUser#user.user_data } || User<-NewUsers ],
      hub_loop(State#hub_state{users = NewUsers})

  end.
