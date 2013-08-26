# Introduction

__Livefrog__ is a [Racket](http://racket-lang.org) program, used to
convert the XML files created by
[ljdump](https://github.com/ghewgill/ljdump), for
[Frog](https://github.com/greghendershott/frog/) usage.


# Installation

__Livefrog__ is available via Racket's
[Planet2](http://pkg.racket-lang.org).

```
raco pkg install livefrog
```

If that doesn't work, install the dependencies, and __Livefrog__
itself, from the local disk.

```
git clone http://github.com/jbclements/sxml.git
git clone http://github.com/ebzzry/livefrog.git
raco pkg install sxml/ livefrog/
```

The trailing slashes are important, to tell `raco` that you are
installing from local directories. Without it, it will try to fetch
the sources from the internet.


# Frog Usage

This sections contains instructions for creating files suitable for
use with Frog.

To create a Markdown file from the file L-10;

```
raco livefrog --frog-markdown L-10
```

To create a file named `disqus.xml` that will be used for importing
comments with Disqus:

```
raco livefrog --disqus disqus.xml
```


# Generic Usage

This section contains instructions for creating files, without a
specific format, using Scribble as an intermediary format.

To convert the file named `L-10` to `L-10.scrbl`:

```
raco livefrog L-10
```

Like above, but in addition to generating `L-10.scrbl`, render it to
`L-10.html` as well, as if by running `scribble --html L-10.scrbl`.

```
raco livefrog --html L-10
```

Again, like above, but in addition to generating `L-10.scrbl`, render
it to `L-10.md` as well, as if by running `scribble --markdown
L-10.scrbl`.

```
raco livefrog --markdown L-10
```


To display the list of available command line options and switches.

```
raco livefrog --help
```


# Updating

If you installed __Livefrog__ using the first method described in the
section *Introduction*, you can update it by running:

```
raco pkg update livefrog
```

However, if you used the latter method, you may update it by pulling
the updates, uninstalling __Livefrog__, then installing it
again:

```
cd livefrog
git pull origin master
cd ..
raco pkg remove livefrog
raco pkg install livefrog/
```


# Miscellany

To reduce typing, you may optionally create an alias to `raco
livefrog` in your shell.

Sh-like shells:
```
echo 'alias livefrog="raco livefrog"' >> ~/.bashrc
```

Csh-like shells:
```
echo 'alias livefrog raco livefrog' >> ~/.cshrc
```

Replace `.bashrc`, and `.cshrc`, with the appropriate init file for
your shell.


# Todo

0. Create tests.
0. Enable HTML inlining.
