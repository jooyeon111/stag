rtl:
	sbt "runMain stag.MainApp IS{4x4}x{4x4}x4.cfg"

test:
	sbt "test"

clean:
	if [ -d $(CURDIR)/output/ ]; then \
		rm -rf $(CURDIR)/output/*; \
	else \
	  echo " There is no test_run_dir directory"; \
  	fi