watch:
	dune build example/main.bc.js --watch

run:
	dune build example/main.bc.js
	cp _build/default/example/main.bc.js ./_static/index.js
	pushd ./_static; python -m SimpleHTTPServer; popd

test:
	dune runtest
