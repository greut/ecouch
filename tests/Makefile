include ../support/include.mk

logdir:
	(mkdir -p logs/)

test: logdir
	
	erl -sname ct \
	-pa $(FULL_PATH_TO_EBIN) \
	-s ct_run script_start \
	-s erlang halt \
	-logdir logs \
	-dir .

clean_logs:
	rm -rf logs

clean:
	rm -rf $(TEST_FILES)
