-module(bits2bytes).
-export([convert/2, convert/3]).

convert(InFileName, OutFileName) ->
	convert(InFileName, OutFileName, 0).
convert(InFileName, OutFileName, SkipBytes) ->
	{ok, InFile} = file:open(InFileName, [read, binary, raw, read_ahead]),
	{ok, OutFile} = file:open(OutFileName, [write, raw]),
	case SkipBytes of
		0 -> ok;
		_ -> file:read(InFile, SkipBytes)
	end,
	process_bytes(InFile, OutFile),
	file:close(OutFile),
	file:close(InFile).

process_bytes(InFile, OutFile) ->
	case file:read(InFile, 8) of
		{ok, Bytes} when byte_size(Bytes) =:= 8 ->
			file:write(OutFile, process_bits(Bytes)),
			process_bytes(InFile, OutFile);
		_ -> ok
	end.

process_bits(<<>>) -> <<>>;
process_bits(<<0:7, Bit:1, Rest/binary>>) ->
	<<Bit:1, (process_bits(Rest))/bits>>.
