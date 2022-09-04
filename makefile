.PHONY: test
.DEFAULT_GOAL: test

venv:
	# virtualenv -p /usr/local/bin/python3.10 .venv
	conda install -v python=3.10.4 -y
	conda create --prefix $(pwd)/.venv python=3.10.4 -y
	conda activate $(pwd)/.venv
	conda install -v numpy=1.23.0 -y
	conda install -v pandas=1.4.4 jupyterlab notebook -y

init:
	pip install -r configs/requirements.txt

update:
	conda env update --prefix ./env --file configs/environment.yml  --prune
#code-convention:
	#flake8
	#pycodestyle

#clean:
#	rm -rf .coverage .pytest_cache reports

#cache-clean:
#	find . -name "*.pyc" -delete
#	find . -type d -name __pycache__ -delete
