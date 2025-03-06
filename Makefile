IS_CONFIG := input_stationary.cfg
OS_CONFIG := output_stationary.cfg
WS_CONFIG := weight_stationary.cfg

.PHONY: all
all:
	sbt "runMain stag.Main ${IS_CONFIG_FILE}"
	sbt "runMain stag.Main ${OS_CONFIG_FILE}"
	sbt "runMain stag.Main ${WS_CONFIG_FILE}"


clean:
	rm -rf $(CURDIR)/*.sv

	if [ -d $(CURDIR)/output/ ]; then \
		rm -rf $(CURDIR)/output/*; \
	else \
	  echo "There is no output directory"; \
  	fi
