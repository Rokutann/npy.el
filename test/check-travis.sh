echo "The default venv:"
pipenv --where
pipenv --venv
echo "Exiting from the default venv..."
exit

echo "project1:"
(cd /tmp/npy-playground/project1/ && pipenv --where)
(cd /tmp/npy-playground/project1/ && pipenv --venv)

echo "project2:"
(cd /tmp/npy-playground/project2/ && pipenv --where)
(cd /tmp/npy-playground/project2/ && pipenv --venv)

echo "project3:"
(cd /tmp/npy-playground/project3/ && pipenv --where)
(cd /tmp/npy-playground/project3/ && pipenv --venv)
