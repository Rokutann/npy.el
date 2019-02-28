# Changelog

## 0.1.6 (wip)

* Adding info-lookup for Python documents.
* Adding a feature to activate Pipenv virtualenvs dynamically.


## 0.1.5 (2019-02-25)

### New features

* Added a scratch buffer facility for `python-mode` with access to Pipenv virtualenvs. By calling `npy-scratch`, you can spawn a python scratch buffer associated or associatable with a Pipenv virtualenv.

### Changes

* Changed `nalist-initialize` from a function to a macro.
* Dropped the customization support for project detection method. It always uses `calling` for Pipenv project root detections and `exploring` for Pipenv virtualenv root detections.

## 0.1.4 (2019-02-22)

### Changes

* Added an optional argument `dedicated` to `npy-run-python`.
* Added testing.

## 0.1.3 (2019-02-04)

### Changes

* Changed the mode name from npipenv to npy.


## 0.1.2 (2019-02-02)

### New features

* Added virtualenv-buffer-dedicated inferior python processes.


## 0.1.1 (2019-01-28)

### New features

* Added new project detection method `exploring` which searches the directory structure bottom-up for a Pipfile. This method is faster than 'calling, speeding up the Emacs startup process when you restore some Python files with desktop-mode.
* Made the project detection method selectable: `exploring` (default) or `calling`.
* Added support for the `pipenv --where` command.

### Changes

* Changed the `package-requires` in the header.

