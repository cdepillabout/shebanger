# shebanger

`shebanger` is a small CLI tool to transform a _boring_ shell script into an
**exciting** series of shebang lines!

`shebanger` takes a shell script as input, and outputs a series of shell
scripts. Each of the output shell scripts only consists of a shebang line, they
don't have any code in their body.  Executing the first output shell script will
run your original input shell program.

## Installation

This section lists installation instructions for various Linux distros.

### Generic Linux

There is a statically-linked x86\_64 Linux ELF binary for `shebanger` available on each of
the [GitHub Releases](https://github.com/cdepillabout/shebanger/releases).

### Nix / NixOS

`shebanger` can be built with Nix by using the `.nix` files in this repo:

```console
$ nix-build
```

You can find the `shebanger` binary in `./result/bin/shebanger`.

You can also install `shebanger` from Nixpkgs by using the `haskellPackages.shebanger`
derivation:

```console
$ nix-build '<nixpkgs>' -A haskellPackages.shebanger
```

### Other Distros

PRs are welcome adding instructions for installing `shebanger` on other Linux
distributions!

## Usage

`shebanger` assumes it will be available on your `PATH`, and be called
`shebanger`.  You will likely see errors with the following commands if
`shebanger` can't be found on your `PATH` or is not named exactly `shebanger`.

There is an example shell script to use with `shebanger` in this repo called
[`test.sh`](./test.sh):

```console
$ cat test.sh
#!/usr/bin/env bash

echo "arguments to this script: $@"
...
```

First, run `shebanger` on this script:

```console
$ shebanger ./test.sh
```

This outputs a bunch of scripts named like the following:

```console
$ ls test.sh.shebanged*
test.sh.shebanged
test.sh.shebanged.1
test.sh.shebanged.2
test.sh.shebanged.3
test.sh.shebanged.4
test.sh.shebanged.5
...
```

Each of these scripts is just a single line, and they look like the following:

```console
$ cat ./test.sh.shebanged
#!/usr/bin/env -S shebanger exec IyEvdXNyL2Jpbi9lbnYgYmFzaAoKZWNobyAiYXJndW1lbnRzIHRvIHRoaXMgc2NyaXA=
```

You can see that this is a script that only consists of a shebang line.  The
contents of the original script are split into 50-byte chunks, base-64 encoded,
passed within the shebang lines of the output scripts.

If you run `./test.sh.shebanged`, the original `test.sh` will be reconstructed
and executed:

```console
$ ./test.sh.shebanged hello this is an argument
arguments to this script: hello this is an argument
...
```

## How Does `shebanger` Work?

`shebanger` works by base-64-encoding the original input script, and splitting
it up into the shebang lines of a series of output scripts.  Each of these
output scripts consist of only a shebang line.

When running the output script, by executing each output script one-by-one in
turn, `shebanger` collects the contents of the original script, reconstructs
it, and executes it.

`shebanger` actually doesn't require the input to be a script, it works on any
executable.  However, see the following sections on limitations of this.

## Limitations

Currently `shebanger` collects the contents of the original scripts into an
environment variable, and passes that along when `exec`'ing the next script.

This means that `shebanger` is not able to execute scripts longer than a few
hundred kilobytes, since they tend to overflow the max size of all environment
variables.

## Inspiration

`shebanger` is inspired by [bangscript](https://github.com/viperML/bangscript)
by [@viperML](https://github.com/viperML).  The `About` explanation of `bangscript` is:

> Embed scripts in a shebang

When I read this, I immediately thought it would be what `shebanger` currently
is.  Since `bangscript` is instead something actually useful, I knew I needed to write
`shebanger`.

## FAQs

1.  **Q: Why would someone ever actually use `shebanger`?**

    A: ...
