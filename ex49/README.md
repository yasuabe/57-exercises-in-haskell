# Memo
## Prerequisite
```
$ dpkg -s libgtk-3-dev | head -n 2
Package: libgtk-3-dev
Status: install ok installed
```
## How to Run
```
$ stack run ex49
...
$ stack run ex49 -- +RTS -N -RTS # (Optional)
...
```