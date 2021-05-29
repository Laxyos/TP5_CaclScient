SHELL = /bin/sh
sv: sources
	(cd sources;make)
clean:
	(cd sources;make clean)

