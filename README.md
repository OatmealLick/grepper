# GREPPER - GREP, but bettER

Better grep for future generations.

## How it works

**GREPPER** is a simplified grep-like application used to find patterns in given texts. Contrarily to classic grep, **GREPPER** will search for all fragments matching the given regular expression - not only for lines containing matching fragment.

**GREPPER** supports limited regular expression syntax which is based on formal definition from formal language theory. That means that **GREPPER** supports following operations:

* concatenation - this operation has no explicit operator, but is done whenever 2 strings in a regular expression are concatenated (e.g. "ab" is a concatenation of "a" and "b"),
* alternative (`+`) - this operation means that one of the arguments on either side of the _plus_ `+` is expected to be found in the matched fragment (equivalent to POSIX `|` operator)
* Kleene star (`*`) - this operation means that the argument to the left side of the _star_ `*` can occur 0 or more times in the matched fragment (equivalent to POSIX `*` operator)

Expressions can be grouped inside parentheses `(...)` forming a unit which is then treated as singular expression - the same can be done using POSIX regexps.

Highest priority is given to the `*` operator, then it is `+` and concatenation is the last. For instance:

```
a+b*c <=> (a+(b*))c
```

**GREPPER** so far does not support escaping mechanism, so you can not pattern match against `(`, `)`, `*`, `+` and `.` signs (last one is used for internal processing of regexps).

**GREPPER** uses concurrency to:
1. Split analyzed file into parts at the beginning of the search and perform search concurrently in all those parts.
2. Spawn processes on "decision nodes" to prevent searching algorithm from backtracking - whenever _alternative_ or _multiplication (Kleene star)_ is encountered the search splits into 2 separate processes running along different paths in the NFA graph modelling the regular expression.

Building own regexp micro-engine we based on [Thompson approach to regexps](https://swtch.com/~rsc/regexp/regexp1.html) as well as the explanation of regular expressions presented in J.E. Hopcroft, R.Motwani, J.D. Ullman "Introduction to Automata Theory, Languages and Computation".

## Setup
**GREPPER** uses rebar3 to for the build process. rebar3 can be downloaded using:

    $ wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

## Run

Before running **GREPPER** in the `apps/grepper/src/grepper.app.src` file three environment variables should be specified:
* `file` - path to the file which content will be used for search
* `regex` - regular expression to be searched for
* `parts` - number of parts in which the `file` will be split in the beginning (each part is searched concurrently)

Once the environment variables have been set, in the main project directory run:
```
$ rebar3 shell
```
This will automatically run the **GREPPER** application with settings specified in environment variables.

**GREPPER** can be also run in `apps/grepper` directory with:
```
$ rebar3 shell
> application:start(grepper)
```
If **GREPPER** should be run in this way the file path in `file` environmental variable must be changed to `resources/file.txt`.

To run EUnit tests go to `apps/grepper` directory and run:
```
$ rebar3 eunit
```

## Authors

Jakub Gwizdała, [Bzdeco](https://github.com/Bzdeco)

Łukasz Ściga, [OatmealLick](https://github.com/OatmealLick)