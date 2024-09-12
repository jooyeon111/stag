rtl:
	sbt "runMain stag.MainApp input_stationary.cfg"
	sbt "runMain stag.MainApp output_stationary.cfg"
	sbt "runMain stag.MainApp weight_stationary.cfg"


test:
	sbt "test"

clean:

	rm -rf $(CURDIR)/*.sv

	if [ -d $(CURDIR)/output/ ]; then \
		rm -rf $(CURDIR)/output/*; \
	else \
	  echo "There is no output directory"; \
  	fi