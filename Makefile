all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd tests;$(MAKE) clean)

test: all
	(cd tests;$(MAKE) test)
	
clean_logs:
	(cd tests;$(MAKE) clean_logs)