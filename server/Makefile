all:
	erlc *.erl

run:
	erl -noshell -boot start_sasl -sname server_controller -setcookie mediacontrollercookie -config server_controller -s server_controller start -s init stop
