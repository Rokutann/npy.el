# Changelog

## 0.1.1 (2019-01-28)

### New features

* Added new project detection method `exploring` which searches the directory structure bottom-up for a Pipfile. This method is faster than 'calling, speeding up the Emacs startup process when you restore some Python files with desktop-mode.
* Made the project detection method selectable: `exploring` (default) or `calling`.
* Added support for the `pipenv --where` command.

### Changes

* Changed the `package-requires` in the header.

