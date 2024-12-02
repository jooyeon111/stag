test:
	sbt "Test / runMain stag.TestMain"

rtl_test:
	sbt "runMain stag.Main input_stationary.cfg"


clean:

	rm -rf $(CURDIR)/*.sv

	if [ -d $(CURDIR)/output/ ]; then \
		rm -rf $(CURDIR)/output/*; \
	else \
	  echo "There is no output directory"; \
  	fi
