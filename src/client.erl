-module(client).

-export([new_client/1]).

-record(client_state, {
    id,
    reg = false,
    nick,
    position,
    angle,
    buffer = <<>>,
    socket,
    score
  }).

-define (PROTO_NICK, 0).
-define (PROTO_REG_COMMIT, 90).
-define (PROTO_REG_ACK, 91).
-define (PROTO_MOVE, 100).
-define (PROTO_AMSG, 110).
-define (PROTO_SMSG, 111).
-define (PROTO_NEW_USER, 120).
-define (PROTO_DROPPED_USER, 121).

new_client(Socket) ->
  recv_loop(#client_state{socket = Socket}).

pack_position([]) -> [];
pack_position([H|T]) -> [<<H:32/signed>> | pack_position(T)].

send_data(MyState, OtherState, Type, Data) ->
  Socket = MyState#client_state.socket,
  % Packet assembly is a tad ugly, but gen_tcp:send will go through the list
  % passed to it and send every item. Assembling lists is cheaper than
  % assembling new binaries with the whole message
  case Type of
    reg_ack ->
      gen_tcp:send(Socket, [ <<?PROTO_REG_ACK:16>> ]);
    move ->
      gen_tcp:send(Socket, [ <<?PROTO_MOVE:16>>, pack_position(OtherState#client_state.position), <<(OtherState#client_state.angle):16/signed>> ]);
    amsg ->
      gen_tcp:send(Socket, [ <<?PROTO_AMSG:16, (OtherState#client_state.id):32, (size(Data)):16>>, Data ]);
    smsg ->
      gen_tcp:send(Socket, [ <<?PROTO_SMSG:16, (size(Data)):16>>, Data ]);
    new_user ->
      Nick = list_to_binary(OtherState#client_state.nick),
      gen_tcp:send(Socket, [ <<?PROTO_NEW_USER:16, (OtherState#client_state.id):32, (size(Nick)):8>>, Nick ]);
    dropped_user ->
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
              io:format("Client registered~p.~n", [NewState]),
              send_data(NewState, undefined, reg_ack, []),
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
          whereis(hub) ! { broadcast, move, self(), [ NewState ] },
          handle_data(NewState);

        <<?PROTO_AMSG:16, Length:16>> when size(Buffer) >= (Length+2) ->
          <<_:16, _:16, Msg:Length/binary, Rest/binary>> = Buffer,
          NewState = State#client_state{buffer = Rest},
          whereis(hub) ! { broadcast, amsg, self(), [ NewState, Msg ] },
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
          whereis(hub) ! { dropped_user, State#client_state.id };
        false ->
          true
      end,
			io:format("Client disconnected ~p.~n", [State]);
    {data_out, Type, OtherState, Data} ->
      send_data(State, OtherState, Type, Data),
			recv_loop(State)

	end.
