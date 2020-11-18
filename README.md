tt
==
A more-unixy twtxt client.

Doesn't force you to maintain a master "following" file (the way Twitter does),
but let's you point it to any follow-file every time it runs.

This means unlimited possibilities for ad-hoc, interesting filtering
combinations. Especially when paired with
[process substitution](https://en.wikipedia.org/wiki/Process_substitution).

![Screenshot](screenshot-multi.jpg)

instructions
------------

### requirements
[Racket](https://download.racket-lang.org/)

### installation
`make install` or `raco pkg install`

### usage
`tt (FOLLOW-FILE)`
