#bencode

Bencode Erlang Library Implementation

## Build

    $ rebar3 compile

## Usage

You can encode Erlang terms to Bencoded string:

    > {ok, E} = bencode:encode([1, 2, 3, mystring]).
	{ok,<<"li1ei2ei3e8:mystringe">>}
    > E.
	<<"li1ei2ei3e8:mystringe">>

You can also decode Bencoded string to Erlang terms:

    > {ok, D} = bencode_decode(B).
	{ok,[1,2,3,<<"mystring">>]}
	> D.
	[1,2,3,<<"mystring">>]

If you want to parse torrent file:

    > {ok, Torrent} = file:read_file("/path/to/my.torrent").
	> {ok, Decoded} = bencode:decode(Torrent).
	> Decoded.

## Todo

 - Decoding options
 - Encoding options
 - Benchmarking
 - More documentation
 - More Test Unit
 - More Common Test
 - Find better way to order keys in dictionary
 
## References

 - https://wiki.theory.org/BitTorrentSpecification#Bencoding
 - http://fileformats.wikia.com/wiki/Torrent_file
 - https://github.com/jlouis/benc
 - https://en.wikipedia.org/wiki/Bencode
