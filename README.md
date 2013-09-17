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


# Usage

This sections contains instructions for creating files suitable for
use with Frog.

To create a Markdown file from the file L-10;

```
raco livefrog --markdown L-10
```

To create a file, named `comments.xml` that will be used for importing
comments, at [http://import.disqus.com/](http://import.disqus.com/),
using `foo.bar.com` as the root site:

```
raco livefrog --site foo.bar.com --disqus comments.xml
```

To create a "generic" Scribble (one that is free-form):

```
raco livefrog --generic-scribble L-10
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
