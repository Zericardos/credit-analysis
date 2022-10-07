.PHONY: test
.DEFAULT_GOAL: test

venv:
	conda install -v python=3.10.4 -y
	conda create --prefix $(pwd)/.venv python=3.10.4 -y
	conda activate $(pwd)/.venv
	conda install -v numpy=1.23.0 -y
	conda install -v pandas=1.5.0 jupyterlab notebook -y

init:
	pip install -r requirements.txt

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
