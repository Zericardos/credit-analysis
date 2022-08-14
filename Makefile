.PHONY: test
.DEFAULT_GOAL: test

venv:
	# virtualenv -p /usr/local/bin/python3.10 .venv
	conda install -v python=3.10.4 -y
	conda create -n env_python3_10 -p $(pwd)/.venv python=3.10.4 -y
	conda activate env_python3_10
	conda install -v pandas=1.4.3 jupyterlab notebook -y

init:
	pip install -r requirements.txt

#code-convention:
	#flake8
	#pycodestyle

#clean:
#	rm -rf .coverage .pytest_cache reports

#cache-clean:
#	find . -name "*.pyc" -delete
#	find . -type d -name __pycache__ -delete
