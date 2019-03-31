## Pipenv projects
mkdir -p /tmp/npy-playground/project1
mkdir -p /tmp/npy-playground/project2
(cd /tmp/npy-playground/project1 && pipenv install)
(cd /tmp/npy-playground/project2 && pipenv install)
mkdir -p /tmp/npy-playground/pipenv-a
mkdir -p /tmp/npy-playground/pipenv-b
(cd /tmp/npy-playground/pipenv-a && pipenv install)
(cd /tmp/npy-playground/pipenv-b && pipenv install)

# Project directories
mkdir -p /tmp/npy-playground/project1/test
mkdir -p /tmp/npy-playground/project1/lib
mkdir -p /tmp/npy-playground/project2/deep/in/the/project

# Non Pipenv projects
mkdir -p /tmp/npy-playground/project3
