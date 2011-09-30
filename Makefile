all:
	mkdir -p ebin
	cp src/gm.app.src ebin/gm.app
	erlc -pa . -o ebin/ src/*.erl

test:
	erlc -DTEST -pa . -o ebin/ src/*.erl
	erl -pa ebin/ -eval 'eunit:test(gm), init:stop().'

console:
	erl -pa ebin/

full: all test
