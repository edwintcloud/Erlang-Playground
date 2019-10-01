run:
	erl -make
	erl -env ERL_LIBS "../" -eval "application:start(ppool), application:start(erlcount)."

args_example:
	erl -make
	erl -env ERL_LIBS "../" -eval "application:start(ppool), application:start(erlcount)." \
		-erlcount directory '"/home/ecloud/dev"' regex '["the", "shit"]' max_files 50