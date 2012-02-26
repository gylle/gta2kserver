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

-module(client).

-export([new_client/1]).

-record(client_state, {
    id,
    reg = false,
    nick,
    position,
    size,
    angle,
    buffer = <<>>,
    socket,
    score
  }).

-define (PROTO_NICK, 0).
-define (PROTO_REG_COMMIT, 90).
-define (PROTO_REG_ACK, 91).
-define (PROTO_MOVE, 100).
-define (PROTO_RESIZE, 101).
-define (PROTO_AMSG, 110).
-define (PROTO_SMSG, 111).
-define (PROTO_NEW_USER, 120).
-define (PROTO_DROPPED_USER, 121).

new_client(Socket) ->
  recv_loop(#client_state{socket = Socket}).

pack_xyz([]) -> [];
pack_xyz([H|T]) -> [<<H:32/signed>> | pack_xyz(T)].

send_data(State, Type, Data) ->
  Socket = State#client_state.socket,
  % Packet assembly is a tad ugly, but gen_tcp:send will go through the list
  % passed to it and send every item. Assembling lists is cheaper than
  % assembling new binaries with the whole message
  case Type of
    reg_ack ->
      gen_tcp:send(Socket, [ <<?PROTO_REG_ACK:16>> ]);
    move ->
      { OtherState } = Data,
      gen_tcp:send(Socket, [ <<?PROTO_MOVE:16, (OtherState#client_state.id):32>>, pack_xyz(OtherState#client_state.position),
          <<(OtherState#client_state.angle):16/signed>> ]);
    resize ->
      { OtherState } = Data,
      gen_tcp:send(Socket, [ <<?PROTO_RESIZE:16, (OtherState#client_state.id):32>>, pack_xyz(OtherState#client_state.size) ]);
    amsg ->
      { OtherState, Msg } = Data,
      gen_tcp:send(Socket, [ <<?PROTO_AMSG:16, (OtherState#client_state.id):32, (size(Msg)):16>>, Msg ]);
    smsg ->
      gen_tcp:send(Socket, [ <<?PROTO_SMSG:16, (size(Data)):16>>, Data ]);
    new_user ->
      { OtherState } = Data,
      Nick = list_to_binary(OtherState#client_state.nick),
      gen_tcp:send(Socket, [ <<?PROTO_NEW_USER:16, (OtherState#client_state.id):32, (size(Nick)):8>>, Nick ]);
    dropped_user ->
      [ OtherState ] = Data,
      gen_tcp:send(Socket, <<?PROTO_DROPPED_USER:16, (OtherState#client_state.id):32>>)
  end.

get_id() ->
  whereis(hub) ! { new_user_id, self() },
  receive
    { new_user_id, Id } ->
      Id
  end.

handle_data(State) ->
  Buffer = State#client_state.buffer,
  case State#client_state.reg of

    false ->
      % Registering phase
      case Buffer of
        <<?PROTO_NICK:16, Length:8, _/binary>> when size(Buffer) >= (Length+3) ->
            <<_:16, _:8, Nick:Length/binary, Rest/binary>> = Buffer,
            handle_data(State#client_state{buffer = Rest, nick = binary_to_list(Nick)});

        <<?PROTO_REG_COMMIT:16, Rest/binary>> ->
          % Aquire user id
          Id = get_id(),
          % Do registration
          NewState = State#client_state{buffer = Rest, id = Id, reg = true},
          whereis(hub) ! { new_user, self(), Id, NewState },
          receive
            { new_user_ack } ->
              io:format("Client registered: ~s(~B).~n", [NewState#client_state.nick, NewState#client_state.id]),
              send_data(NewState, reg_ack, []),
              handle_data(NewState)
          end;

        _Else -> % No matching pattern
          io:format("No match for ~p ~n", [Buffer]),
          State

      end;

    true ->
      % Game phase
      case Buffer of

        % TODO: rate limit

        <<?PROTO_MOVE:16, Pos_x:32/signed, Pos_y:32/signed, Pos_z:32/signed, Angle:16/signed, Rest/binary>> ->
          NewState = State#client_state{buffer = Rest, position = [Pos_x, Pos_y, Pos_z], angle = Angle},
          whereis(hub) ! { broadcast, move, self(), { NewState } },
          handle_data(NewState);

        <<?PROTO_RESIZE:16, Size_x:32/signed, Size_y:32/signed, Size_z:32/signed, Rest/binary>> ->
          NewState = State#client_state{buffer = Rest, size = [Size_x, Size_y, Size_z]},
          whereis(hub) ! { broadcast, resize, self(), { NewState } },
          handle_data(NewState);

        <<?PROTO_AMSG:16, Length:16, _/binary>> when size(Buffer) >= (Length+2) ->
          % TODO: max length 1023
          <<_:16, _:16, Msg:Length/binary, Rest/binary>> = Buffer,
          NewState = State#client_state{buffer = Rest},
          whereis(hub) ! { broadcast, amsg, self(), { NewState, Msg } },
          io:format("amsg: <~s(~B)> '~s'.~n", [NewState#client_state.nick, NewState#client_state.id, Msg]),
          handle_data(NewState);

        _Else -> % No matching pattern
          State


      end
  end.

handle_data(State, NewData) ->
  % Assemble buffer
  Buffer = <<(State#client_state.buffer)/binary, NewData/binary>>,
  handle_data(State#client_state{buffer = Buffer}).

recv_loop(State) ->
  Socket = State#client_state.socket,
	% reset the socket for flow control
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Data} ->
      NewState = handle_data(State, Data),
			recv_loop(NewState);
		{tcp_closed, Socket} ->
      case State#client_state.reg of
        true ->
          whereis(hub) ! { dropped_user, State#client_state.id },
          io:format("Client disconnected: ~s(~B).~n", [State#client_state.nick, State#client_state.id]);
        false ->
          io:format("Unregistered client disconnected.~n"),
          true
      end;
    {data_out, Type, Data} ->
      send_data(State, Type, Data),
			recv_loop(State)
	end.
