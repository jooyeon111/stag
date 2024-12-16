test:
	sbt "Test / runMain stag.TestMain input_stationary.cfg"

rtl_test:
	sbt "runMain stag.Main input_stationary.cfg"


rtl_gen:
	sbt "runMain stag.Main is_{256x4}x{1x1}x1.cfg"


clean:

	rm -rf $(CURDIR)/*.sv

	if [ -d $(CURDIR)/output/ ]; then \
		rm -rf $(CURDIR)/output/*; \
	else \
	  echo "There is no output directory"; \
  	fi
