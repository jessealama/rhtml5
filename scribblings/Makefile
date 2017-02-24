.PHONY: clean all

all:
	# nothing to do

clean:
	find . -mindepth 1 -maxdepth 1 -type f -name '*~' -delete
	find . -mindepth 1 -maxdepth 1 -type d -exec basename {} ';' | parallel --jobs=1 --halt-on-error=1 $(MAKE) -C {} clean
