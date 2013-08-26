# Introduction

__Livefrog__ is a [Racket](http://racket-lang.org) program, used to
convert the XML files created by
[ljdump](https://github.com/ghewgill/ljdump), or
[ljmigrate](https://github.com/ceejbot/ljmigrate), to
[Scribble](http://docs.racket-lang.org/scribble/), and the output
formats that Scribble supports. Input files are categorized as either
*entry*, or *comment* data, referring to the posts created by the
owner of the journal, and comments by journal viewers, respectively.


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

To convert the file named `file.xml` to `file.scrbl`:

```
raco livefrog file.xml
```

Like above, but in addition to generating `file.scrbl`, render it to
`file.html` as well, as if by running `scribble --html file.scrbl`.

```
raco livefrog --html file.xml
```

Again, like above, but in addition to generating `file.scrbl`, render
it to `file.md` as well, as if by running `scribble --markdown
file.scrbl`.

```
raco livefrog --markdown file.xml
```


To display the list of available command line options and switches.

```
raco livefrog -h
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

0. Enable [Frog](https://github.com/greghendershott/frog) markdown output.
0. Create tests.
0. Enable HTML inlining.
