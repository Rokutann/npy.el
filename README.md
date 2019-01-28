
## Synopsis

**nPipenv** is a nano (or minimalistic) support for Pipenv in Emacs. Its
goal is to provide a nice integration of Pipenv virtual environment
and Emacs inferior python mode.  For instance - spawning multiple
inferior python processes for multiple Pipenv projects, and
dispatching Python code chunks sent from a buffer, which is visiting a Python file
in one of such Pipenv projects, to the inferior python process bound to
the same Pipenv project. We call this new type of inferior python processes
**virtualenv-dedicated inferior python process**.

Some of nPipenv's features:

* spawn a virtualenv-dedicated inferior python process for a Pipenv project
* send Python code chunks from multiple Python file buffers in a Pipenv project
  to the virtualenv-dedicated inferior python process for the project
* dispatch Python code chunks to the appropriate virtualenv-dedicated inferior
  python process when you have spawned multiple virtualenv-dedicated inferior
  python processes for different Pipenv projects
* change buffer's association to a Pipenv project automatically when
  you write out the content of the file buffer onto a directory
  under a different Pipenv project
* spawn a Pipenv shell for a Pipenv project

## Installation

Ensure you have two elisp packages, `f` and `s`, installed in your Emacs
prior to the nPipenv installation. You can get them from `MELPA`
or `Github`.

Place `npipenv.el` on a directory in your `load-path', and add this to
your Emacs config:

```el
(require 'npipenv)
(npipenv-initialize)
```

The initial keymap prefix "C-c '" is just a suggestion. Feel free to
customize `npipenv-keymap-prefix` to whatever works best for you.

### Basic Usage

`nPipenv-mode` is automatically enabled when you open a Python file, i.e.
it's hooked to the `python-mode`. You can explicitly
activate it for buffers in other modes by <kbd>M-x</kbd> <kbd>npipenv-mode</kbd>.

The mode line shows the virtualenv associated with the file. The association usually
happens when you open a file. The mode line looks like **nP[v:hello_world]**
where 'hello_world' is the name of the Pipenv project the file belongs to.

Spawn an virtualenv-dedicated inferior python process by <kbd>C-c</kbd> <kbd>'</kbd>
<kbd>p</kbd> or <kbd>M-x</kbd> <kbd>npipenv-run-python</kbd>. Its buffer name should
be **\*Python[v:hello_world]\***.

After that npipenv-mode works transparently: you can use all the
interaction between the source file(s) and the inferior python mode as
in `python-mode`.

Note: The `python-mode` defines two types of inferior python
processes: global and (buffer-)dedicated. The precedence order for the
destination when you send a source code chunk from a file buffer in
a situation that you have spawned both a global inferior process and a
dedicated inferior process is 'always dedicated first.' With adding a
new type, virtualenv-dedicated, the current nPipenv's precedence
strategy is 'always virtualenv-dedicated
first'. i.e. virtualenv-dedicated -> buffer-dedicated -> global. We
might change this in a future release.

If you need to update the association between a file buffer and a
virtual environment, use the command <kbd>C-c</kbd> <kbd>'</kbd> <kbd>u</kbd> or <kbd>M-x</kbd>
<kbd>npipenv-update-venv-root</kbd>.

You can change the Pipenv project method by changing
`npipenv-pipenv-project-detection`. The default value is `'exploring`
which explores the directory structure bottom-up to find a
`Pipfile`. Another option is `'calling` which calls `pipenv --where`
command. `'calling` is the surest way but slow.

Spawn a Pipenv shell by <kbd>C-c</kbd> <kbd>'</kbd> <kbd>s</kbd> or <kbd>M-x</kbd>
<kbd>npipenv-shell</kbd>.

## Caveats

* It slows down your Emacs startup process when you restore lots of
  Python files with desktop-mode. How slow depends on the project
  detection mode: `exploring` can be acceptable, 'calling' will be a
  disaster.

## Known issues

To be available when posted.

## Changelog

A changelog is available [here](CHANGELOG.md).

## License

Copyright © 2019 Cyriakus "Mukuge" Hill

Distributed under the GNU General Public License, version 3
