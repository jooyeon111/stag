test:
	sbt "runMain stag.MainApp is_16x16x1x1x4.cfg"
	#sbt "runMain stag.MainApp os_8x8x1x1x1.cfg"
	#sbt "runMain stag.MainApp ws_8x8x1x1x1.cfg"

all:
	sbt "runMain stag.MainApp is_8x8x4x1x2.cfg"
	sbt "runMain stag.MainApp is_16x8x4x1x1.cfg"
	sbt "runMain stag.MainApp is_16x16x2x1x1.cfg"
	sbt "runMain stag.MainApp is_32x16x1x1x1.cfg"

	sbt "runMain stag.MainApp os_4x8x1x1x4.cfg"
	sbt "runMain stag.MainApp os_8x16x1x1x4.cfg"
	sbt "runMain stag.MainApp os_16x16x1x1x2.cfg"
	sbt "runMain stag.MainApp os_16x32x1x1x1.cfg"

	sbt "runMain stag.MainApp ws_8x8x1x4x2.cfg"
	sbt "runMain stag.MainApp ws_8x16x1x4x1.cfg"
	sbt "runMain stag.MainApp ws_16x16x1x2x1.cfg"
	sbt "runMain stag.MainApp ws_16x32x1x1x1.cfg"

clean:

	rm -rf $(CURDIR)/*.sv

	if [ -d $(CURDIR)/output/ ]; then \
		rm -rf $(CURDIR)/output/*; \
	else \
	  echo "There is no output directory"; \
  	fi
