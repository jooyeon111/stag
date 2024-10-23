test:
	#sbt "runMain stag.MainApp input_stationary.cfg"
	#sbt "runMain stag.MainApp output_stationary.cfg"
	#sbt "runMain stag.MainApp weight_stationary.cfg"

all:
	sbt "runMain stag.MainApp is_8x8x1x1x1.cfg"
	sbt "runMain stag.MainApp is_4x4x4x1x1.cfg"
	sbt "runMain stag.MainApp is_4x4x1x1x4.cfg"
	sbt "runMain stag.MainApp is_2x2x16x1x1.cfg"
	sbt "runMain stag.MainApp is_2x2x4x1x4.cfg"
	sbt "runMain stag.MainApp is_2x2x1x1x16.cfg"
	sbt "runMain stag.MainApp is_1x1x16x1x4.cfg"
	sbt "runMain stag.MainApp is_1x1x4x1x16.cfg"

	sbt "runMain stag.MainApp os_8x8x1x1x1.cfg"
	sbt "runMain stag.MainApp os_4x4x2x2x1.cfg"
	sbt "runMain stag.MainApp os_4x4x1x1x4.cfg"
	sbt "runMain stag.MainApp os_2x2x4x4x1.cfg"
	sbt "runMain stag.MainApp os_2x2x2x2x4.cfg"
	sbt "runMain stag.MainApp os_2x2x1x1x16.cfg"
	sbt "runMain stag.MainApp os_1x1x4x4x4.cfg"
	sbt "runMain stag.MainApp os_1x1x2x2x16.cfg"

	sbt "runMain stag.MainApp ws_8x8x1x1x1.cfg"
	sbt "runMain stag.MainApp ws_4x4x1x4x1.cfg"
	sbt "runMain stag.MainApp ws_4x4x1x1x4.cfg"
	sbt "runMain stag.MainApp ws_2x2x1x16x1.cfg"
	sbt "runMain stag.MainApp ws_2x2x1x4x4.cfg"
	sbt "runMain stag.MainApp ws_2x2x1x1x16.cfg"
	sbt "runMain stag.MainApp ws_1x1x1x16x4.cfg"
	sbt "runMain stag.MainApp ws_1x1x1x4x16.cfg"

clean:

	rm -rf $(CURDIR)/*.sv

	if [ -d $(CURDIR)/output/ ]; then \
		rm -rf $(CURDIR)/output/*; \
	else \
	  echo "There is no output directory"; \
  	fi