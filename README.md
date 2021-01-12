# Ada Experiments

Just a small fun project to experiment with [Ada Programming Language](https://www.adaic.org/) and [AWS](https://github.com/AdaCore/aws)

## Quick Start

Dependencies:
- [gprbuild](https://github.com/AdaCore/gprbuild)
- [aws](https://github.com/AdaCore/aws)

On Debian you can install all of the dependencies from the official repos:

```console
$ sudo apt install gprbuild libaws18-dev
```

To build the examples:

```console
$ gprbuild -P ./wttr.gpr
$ ./build/wttr novosibirsk

$ gprbuild -P ./server.gpr
$ ./build/server
$ iexplore.exe http://localhost:8080/

$ gprbuild -P ./freenode.gpr
$ ./build/freenode
```

## Screencasts

| Thumbnail | Link |
| --- | --- |
| [![thumbnail](http://i3.ytimg.com/vi/ljeYMzDThMY/default.jpg)](https://www.youtube.com/watch?v=ljeYMzDThMY) | [Programming in Ada — Part 1](https://www.youtube.com/watch?v=ljeYMzDThMY) |
