watch:
	dune build example/main.bc.js --watch

run:
	dune build example/main.bc.js
	mkdir -p ./_static
	cp _build/default/example/main.bc.js ./_static/index.js
	cp ./example/static/index.html ./_static/index.html
	pushd ./_static; python -m SimpleHTTPServer; popd

test:
	dune runtest
