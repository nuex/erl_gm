all:
	mkdir ebin
	cp src/gm.app.src ebin/gm.app
	erlc -pa . -o ebin/ src/*.erl

test:
	erl -pa ebin/ -eval 'eunit:test(gm), init:stop().'

console:
	erl -pa ebin/

full: all test
