[![Build Status](https://travis-ci.org/mukuge/npy.el.svg?branch=master)](https://travis-ci.org/mukuge/npy.el)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Synopsis

**npy.el** is a set of nano (or minimalistic) extensions to the Python
development support in Emacs. Its first goal is to provide a nice
integration of Pipenv virtual environments and Emacs inferior python
modes.  For instance - spawning multiple inferior python processes for
multiple Pipenv projects, and dispatching Python code chunks sent from
a buffer, which is visiting a Python file in one of the Pipenv
projects, to the inferior python process bound to the virtual
environment of the same Pipenv project. We call this new type of
inferior processes **virtualenv-dedicated inferior python processes**.

Some of npy.el's features:

* spawn a virtualenv-dedicated inferior python process for a Pipenv project
* dispatch Python code chunks from a `python-mode` buffer to the
  appropriate virtualenv-dedicated inferior python process when you
  have spawned multiple virtualenv-dedicated inferior python processes
  for different Pipenv projects
* spawn a viartualenv-dedicated inferior python process associated
  only with a `python-mode` buffer. We call this type of processes
  **virtualenv-buffer-dedicated inferior python processes**
* spawn a python scratch buffer associated or associatable with a
  virtualenv-dedicated or virtualenv-buffer-dedicated inferior python
  process. We call this type of scratch buffers
  **virtualenv-dedicated** or **virtualenv-buffer-dedicated python
  scratch buffers**
* automatically change the activated virtualenv when you switch
  back-and-forth between buffers visiting files in different Pipenv
  projects. This is for other pythonic modes such as `pytest.el` and
  `python-pytest.el`
* Enable `info-lookup-symbol` for the Python official documents. The
  info files for Python version 3.7.2 are included
* spawn a Pipenv shell for a Pipenv project

## Installation

Ensure you have four elisp packages, `f.el`, `s.el`, `nalist.el`, and
`gpc.el`, installed in your Emacs prior to the npy.el
installation. You can get `f` and `s` from `MELPA` or `Github`, and
[nalist](https://github.com/mukuge/nalist.el) and
[gpc](https://github.com/mukuge/gpc.el) from `Github`.

Place `npy.el` on a directory in your `load-path', and add this to
your Emacs config:

```el
(require 'npy)
(npy-initialize)
```

The initial keymap prefix <kbd>C-c '</kbd> is just a suggestion. Feel free to
customize `npy-keymap-prefix` to whatever works best for you.

## Basic Usage

### Enable `npy-mode`

`npy-mode` is automatically enabled when you open a Python file, i.e.
it's hooked to the `python-mode`.

### Spawn Virtualenv-dedicated Inferior Python and Python Scratch buffers

When `npy-mode` is turned on, the mode line shows the Pipenv project
the file the buffer is visiting belongs to.  The lookup for a Pipenv
project in the system usually happens when you visit the file. The
mode line looks like **Py[v:hello_world]** where 'hello_world' is the
name of the Pipenv project.

Spawn an virtualenv-dedicated inferior python process by <kbd>C-c '
p</kbd> or <kbd>M-x</kbd> <kbd>npy-run-python</kbd>. Its buffer name
should be **\*Python[v:hello_world]\***.

Hereafter `npy-mode` works transparently: you can use all the
interaction between buffers visiting Python files and the inferior
python mode as in an ordinary `python-mode` buffer.

Additionally, you can spawn a buffer-dedicated inferior python mode
with access to the virtual environment by putting any prefix args to
<kbd>M-x</kbd> <kbd>npy-run-python</kbd> or <kbd>C-u C-c 'p</kbd>. The
mode lines look like **Py[v:hello_world;b:hello.py]** and
**\*Python[v:hello_world;b:hello.py]\*** respectively.  This type of
inferior python processes only associated with the `python-mode`
buffer where it spawned. Other buffers visiting files in the same
Pipenv project can't send code chunks to the process.

When you write out the content of a buffer visiting a file in a Pipenv
project as a file under a different Pipenv project, `npy-mode`
automatically change the buffer's association to the new Pipenv
project.

Note: The `python-mode` defines two types of inferior python
processes: global and (buffer-)dedicated. Their precedence order is:

```
buffer-dedicated > global
```

The precedence order in npy.el, which introduces two additional two
types: virtualenv-dedicated and virtualenv-buffer-dedicated, is:

```
virtualenv-buffer-dedicated > virtualenv-dedicated > buffer-dedicated > global
```

We might change this order in a future release.

You can spawn a scratch buffer for `python-mode` with access to a
virtualenv-dedicated or virtualenv-buffer-dedicated inferior python
process by <kbd>M-x</kbd> <kbd>npy-scratch</kbd>.

### Activate/deactivate Virtualenvs Dynamically

Enable the feature by <kbd>M-x</kbd>
<kbd>npy-activate-virtualenv-automatic</kbd>. Thereafter, the feature
supplies the appropriate executable search path information to other
pythonic modes such as `pytest.el` and `python-pytest.el` per buffer
base.

Disable the feature by <kbd>M-x</kbd>
<kbd>npy-deactivate-virtualenv-automatic</kbd>.

### Lookup Python Official Documents

Install `python372full.info` in your system. 

Lookup the info file by <kbd>M-x</kbd> <kbd>info-lookup-symbol</kbd>
or <kbd>C-h S</kbd>.

This package contains two info files: `python372full.info` and
`python372api.info` in the data/info directory. `python372api.info`
only contains The Python Language Reference, The Python Standard
Library, and Python/C API Reference Manual.

### Spawn a Pipenv Shell

Spawn a Pipenv shell by <kbd>C-c ' s</kbd> or <kbd>M-x</kbd>
<kbd>npy-shell</kbd>.

## Caveats

* It slows down your Emacs startup process when you restore lots of
  Python files with `desktop-mode`.

## Known issues

To be available when posted.

## Changelog

A changelog is available [here](CHANGELOG.md).

## License

Copyright © 2019 Cyriakus "Mukuge" Hill

Distributed under the GNU General Public License, version 3
