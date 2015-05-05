-module(rds_parser).
-export([parse_file/1]).

-record(state, {af=[], ps={$.,$.,$.,$.,$.,$.,$.,$.}}).
-record(afs, {freq_band=unknown, channels=[]}).
-record(vhf, {no_chans}).

parse_file(FileName) ->
	{ok, F} = file:read_file(FileName),
	Results = try_bits(F),
	io:format("[AF] ~s\n", [format_afs(Results#state.af)]),
	io:format("[PS] ~s\n", [tuple_to_list(Results#state.ps)]),
	Results.

try_bits(Bits) -> try_bits(Bits, #state{}).
try_bits(Bits, State = #state{af=AF, ps=PS}) when bit_size(Bits) >= 26 ->
	NewState = case Bits of
		<<4, 10:6, Idx:2, CheckWord1:10, AF1, AF2, CheckWord2:10, PS1, PS2, CheckWord3:10, _/bits>> ->
			CW1 = check_word(<<4, 10:6, Idx:2>>, CheckWord1, 1),
			CW2 = check_word(<<AF1, AF2>>, CheckWord2, 2),
			CW3 = check_word(<<PS1, PS2>>, CheckWord3, 3),
			case CW1 andalso CW2 andalso CW3 of
				true ->
					io:format("Index: ~p AF1: ~p AF2: ~p, PS: ~p\n",
							  [Idx, format_af(AF1), format_af(AF2), [PS1, PS2]]),
					#state{af=ordsets:union(AF, ordsets:from_list([AF1, AF2])),
						ps=setelement(Idx * 2 + 2, setelement(Idx * 2 + 1, PS, PS1), PS2)};
				_ -> State
			end;
		_ -> State
	end,
	<<_:1, Rest/bits>> = Bits,
	try_bits(Rest, NewState);
try_bits(_, State) -> State.

-define(POLY, 16#5B9).
-define(PLEN, 10).

check_word(Subject, Expected, Pos) ->
	Expected == cwp2(cwp1(Subject)) bxor offset_word(Pos).

cwp1(Subject) -> cwp1(Subject, 0).
cwp1(<<>>, Acc) -> Acc;
cwp1(<<Bit:1, Rest/bits>>, Acc) -> cwp1(Rest, xor_poly(Acc bsl 1 bor Bit)).

cwp2(Reg) -> cwp2(Reg, ?PLEN).
cwp2(Reg, 0) -> Reg band ((1 bsl ?PLEN) - 1);
cwp2(Reg, Iter) -> cwp2(xor_poly(Reg bsl 1), Iter - 1).

xor_poly(X) when X band (1 bsl ?PLEN) =:= 0 -> X;
xor_poly(X) -> X bxor ?POLY.

offset_word(0) -> 252;
offset_word(1) -> 408;
offset_word(2) -> 360;
offset_word(3) -> 436;
offset_word(4) -> 848.

format_afs(AFs) ->
	State = format_afs(AFs, #afs{}),
	{Band, NChan} = State#afs.freq_band,
	io_lib:format("~p ~p channels: ~s", [NChan, Band, format_channels(State#afs.channels)]).
format_afs([], State) -> State;
format_afs([AF | Rest], State) ->
	NewState = case format_af(AF) of
		FB when is_tuple(FB) -> State#afs{freq_band=FB};
		Value when is_integer(Value) -> State#afs{channels=[Value | State#afs.channels]};
		_ -> State
	end,
	format_afs(Rest, NewState).

format_channels(Chans) ->
	string:join([integer_to_list(C) ++ " kHz" || C <- lists:sort(Chans)], ", ").

format_af(0) -> not_used;
format_af(205) -> filler_code;
format_af(AF) when AF >= 206 andalso AF =< 224 -> not_assigned;
format_af(224) -> no_af_exists;
format_af(AF) when AF >= 251 -> not_assigned;
format_af(AF) when AF >= 225 andalso AF =< 249 -> #vhf{no_chans=AF - 224};
format_af(AF) -> (AF + 875) * 100.
