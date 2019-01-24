
## Synopsis

**nPipenv** is a nano (minimalist) support for Pipenv in Emacs. Its
goal is to provide a nice integration of Pipenv virtual environmens
and Emacs inferior python mode.  For instance - sending Python code
chunks from multiple Python sources in a Pipenv project to a single inferior
python process for its virtual environment (**virtualenv-dedicated
inferior python process**). Also, you can spawn multiple
virutalenv-dedicated inferior python processes for multiple Pipenv
projects from a single Emacs process.

Some of nPipenv's features:

* spawn multiple virtualenv-dedicated inferior python processes for
  multiple Pipenv projects
* send Python code chunks from multiple Python sources in a Pipenv project
  to the virtualenv-dedicated inferior python process for the project
* dispatch Python code chunks to the appropriate virtualenv-dedicated inferior
  python process when you have spawned multiple virtualenv-dedicated inferior
  python processes for different Pipenv projects
* change buffer's association to a Pipenv project automatically when
  you write out the file buffer to a directory
  under a differet Pipenv project
* spawn a Pipenv shell for a Pipenv project

## Installation

Ensure you have two pacakges, `f` and `s`, installed in your Emacs
prior to the nPipenv installation. You can install them from `MELPA`
or Github.

Place `npipenv.el` on a directory in your `load-path', and add this to
your Emacs config:

```el
(require 'npipenv)
(npipenv-initialize)
```

The initial keymap prefix "C-c '" is just a suggestion. Feel free to
customize `npipenv-keymap-prefix` to whatever works best for you.

### Basic Usage

`nPipenv-mode` is automatically enabled when you open a Python source
file, i.e. it's hooked to the `python-mode`. You can explicitly
activate it for buffers in other modes by <kbd>M-x</kbd> <kbd>npipenv-mode</kbd>.

The mode line shows the virtualenv the file the buffer is visiting is
 associated at the time the buffer visited the file. It looks like
 **nP[v:hello_world]** where 'hello_world' is the name of the Pipenv
 project the file belongs to.

Spawn an virtualenv-dedicated inferior python process by <kbd>C-c</kbd> <kbd>'</kbd>
<kbd>p</kbd> or <kbd>M-x</kbd> <kbd>npipenv-run-python</kbd>. Its buffer name should
be **\*Python[v:hello_world]\***.

After that npipenv-mode works transparently: you can use all the
interaction between the source file(s) and the inferior python mode as
in `python-mode`.

Note: Originallly, the `python-mode` has two types of inferior python
processes: global and (buffer-)dedicated. The precedence order for the
destination when you send a source code chunk from a source buffer in
a situation that you have spawned both a global inferior process and a
dedicated inferior process is 'always dedicated first.' With adding a
new type, virtualenv-dedicated, the current nPipenv's precedence
strategy is 'always virtualenv-dedicated
first'. i.e. virtualenv-dedicated -> buffer-dedicated -> global. We
might change this in a future release.

If you need to update the association between a file buffer and a
virtual environment, use the command <kbd>C-c</kbd> <kbd>'</kbd> <kbd>u</kbd> or <kbd>M-x</kbd>
<kbd>npipenv-update-venv-root</kbd>.

Spawn a Pipenv shell by <kbd>C-c</kbd> <kbd>'</kbd> <kbd>s</kbd> or <kbd>M-x</kbd>
<kbd>npipenv-shell</kbd>.

## Caveats

* It will slow down your Emacs startup process significantly when you
  restore lots of Python sources by desktop-mode.

## Known issues

To be availabe when posted.

## Changelog

To be availabe at the second release.

## License

Copyright © 2019 Cyriakus "Mukuge" Hill

Distributed under the GNU General Public License, version 3
