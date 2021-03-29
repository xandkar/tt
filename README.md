tt
==
A more-unixy [twtxt](https://github.com/buckket/twtxt) client.

[![Build Status](https://travis-ci.org/xandkar/tt.svg?branch=master)](https://travis-ci.org/xandkar/tt)

Doesn't force you to maintain a master "following" file (the way Twitter does),
but let's you point it to any follow-file every time it runs.

This means unlimited possibilities for ad-hoc, interesting filtering
combinations. Especially when paired with
[process substitution](https://en.wikipedia.org/wiki/Process_substitution).

### long format (default)
![Screenshot](screenshot-long.jpg)

### short format (CLI option: -s)
![Screenshot](screenshot-short.jpg)


instructions
------------

### requirements

#### manual
[Racket](https://download.racket-lang.org/)

#### package manager
- Void Linux: `xbps-install racket`
- Debian: `apt install racket`

### installation
`make build && make install` will build and copy `tt` binary into
`$PREFIX/bin`, where `$PREFIX` defaults to `$HOME`.

### configuration
Put your `<nick>` and `<uri>` into `~/.tt/me`. For example, mine is:

```
$ cat ~/.tt/me
xandkar https://xandkar.net/twtxt.txt
```
It will be used to fill the `User-Agent` header, so that others can tell you're
reading their twtxts and perhaps read yours. This isn't strictly necessary and
if omitted, you'll stay anonymous.

### usage

		$ tt --help
		tt [ <option> ... ] <command> [<args>] ...
		 where <option> is one of
			-d, --debug : Enable debug log level.

		 and <command> is one of
		 r, read     : Read the timeline (offline operation).
		 d, download : Download the timeline.
		 u, upload   : Upload your twtxt file (alias to execute ~/.tt/upload).
		 c, crawl    : Discover new peers mentioned by known peers (offline operation).

			--help, -h : Show this help
			-- : Do not treat any remaining argument as a switch (at this level)
		 Multiple single-letter switches can be combined after one `-'; for
			example: `-h-' is the same as `-h --'

#### download
peers' feeds from the Internet:

		$ tt d -h
		tt download [ <option> ... ] [<file-paths>] ...
		 where <option> is one of
			-j <njobs>, --jobs <njobs> : Number of concurrent jobs.
			--help, -h : Show this help
			-- : Do not treat any remaining argument as a switch (at this level)
		 Multiple single-letter switches can be combined after one `-'; for
			example: `-h-' is the same as `-h --'

#### read
your timeline:

		$ tt r -h
		tt read [ <option> ... ] [<file-paths>] ...
		 where <option> is one of
			-r, --rev : Reverse displayed timeline order.
		/ -s, --short : Short output format
		\ -l, --long : Long output format
			--help, -h : Show this help
			-- : Do not treat any remaining argument as a switch (at this level)
		 /|\ Brackets indicate mutually exclusive options.
		 Multiple single-letter switches can be combined after one `-'; for
			example: `-h-' is the same as `-h --'

`FOLLOW-FILE` contains lines with space-separated nick and twtxt.txt URI, like:

```
xandkar https://xandkar.net/twtxt.txt
```

If omitted, `FOLLOW-FILE` defaults to `~/.tt/peers`.

#### other commands

    tt <command> -h


notes
-----

### LWW downloads
Downloaded timelines are stored in `~/.tt/cache/objects/<URL_ENCODED_URL>`, but
no attempt is made to preserve the previously-downloaded messages - each
download overrites the previous. One of the implications is that authors can
edit/delete history without you noticing.
