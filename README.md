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
`make build && make install` will build and copy `tt` binary into `$PREFIX/bin`.

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
Download feeds from the Internet:
`tt d (FOLLOW-FILE)`

Read your timeline:
`tt r (FOLLOW-FILE)`

`FOLLOW-FILE` contains lines with space-separated nick and twtxt.txt URI, like:

```
xandkar https://xandkar.net/twtxt.txt
```

See the rest of the usage options:
`tt -h`

`tt <command> -h`


notes
-----

### LWW downloads
Downloaded timelines are stored in `~/.tt/cache/<SHA1_OF_URI>`, but no attempt
is made to preserve the previously-downloaded messages - each download
overrites the previous. One of the implications is that authors can edit/delete
history without you noticing.
