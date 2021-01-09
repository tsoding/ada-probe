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
$ iexplorer.exe http://localhost:8080/
```
