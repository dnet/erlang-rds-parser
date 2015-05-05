Erlang RDS parser
=================

A simple parser that extracts AF (alternative frequencies) and PS
(programme service) information from binary RDS stream. Such a stream
can be obtained by either a hardware decoder or using SDR with GNU Radio.

An example for latter can be found in https://github.com/bastibl/gr-rds

License
-------

The whole project is available under MIT license, see `LICENSE.txt`.

Dependencies
------------

 - Recent Erlang distribution

Building and usage
------------------

The input file should contain information in every bit, as opposed to
regular GNU radio output that contains only one bit per byte.
Converting from latter to former can be done using module `bits2bytes`
also included in the source tree.

	1> c(rds_parser).
	{ok,rds_parser}
	2> rds_parser:parse_file("kossuth-fm997-debrecen-octets.bin").
	Index: 2 AF1: 97900 AF2: filler_code, PS: "UT"
	Index: 0 AF1: 103000 AF2: 105900, PS: "KO"
	Index: 2 AF1: 103000 AF2: 105900, PS: "UT"
	Index: 1 AF1: 97900 AF2: filler_code, PS: "SS"
	...
	Index: 1 AF1: {vhf,4} AF2: 97500, PS: "SS"
	Index: 2 AF1: 97900 AF2: filler_code, PS: "UT"
	[AF] 4 vhf channels: 97500 kHz, 97900 kHz, 103000 kHz, 105900 kHz
	[PS] KOSSUT..
	{state,[100,104,155,184,205,228],{75,79,83,83,85,84,46,46}}
	3>
