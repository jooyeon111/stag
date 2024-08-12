rtl:
	sbt "runMain stag.MainApp is_sta_{4x4}x{4x4}x4.cfg"
	sbt "runMain stag.MainApp os_sta_{4x4}x{4x4}x4.cfg"
	sbt "runMain stag.MainApp ws_sta_{4x4}x{4x4}x4.cfg"

test:
	sbt "test"

clean:

	rm -rf $(CURDIR)/*.sv

	if [ -d $(CURDIR)/output/ ]; then \
		rm -rf $(CURDIR)/output/*; \
	else \
	  echo "[error]There is no output directory"; \
  	fi