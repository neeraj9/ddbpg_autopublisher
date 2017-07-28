VERSION = $(BUILD_NUMBER) # THis does not seem to work at the moment
VERSION ?= 0
COMPONENT = ddbpg_autopublisher

.PHONY: prepare package

prepare:
	mkdir -p stage/data/ddbpg_autopublisher/example
	mkdir -p stage/etc/systemd/system
	mkdir -p stage/usr/bin
	mkdir -p stage/usr/lib/ddbpg_autopublisher/lib
	mkdir -p stage/usr/share/ddbpg_autopublisher
	mkdir -p stage/var/lib/ddbpg_autopublisher
	mkdir -p stage/var/log/ddbpg_autopublisher
	mkdir -p stage/DEBIAN
	cp -r ../../_build/deb/rel/ddbpg_autopublisher/bin stage/usr/lib/ddbpg_autopublisher/
	cp -r ../../_build/deb/rel/ddbpg_autopublisher/etc stage/data/ddbpg_autopublisher/etc/
	cp -r ../../_build/deb/rel/ddbpg_autopublisher/erts-* stage/usr/lib/ddbpg_autopublisher/
	tar -cC ../../_build/deb/rel/ddbpg_autopublisher/lib --exclude c_src --exclude src . | tar -xC stage/usr/lib/ddbpg_autopublisher/lib
	cp -r ../../_build/deb/rel/ddbpg_autopublisher/releases stage/usr/lib/ddbpg_autopublisher/
	cp -r ../../_build/deb/rel/ddbpg_autopublisher/share stage/usr/lib/ddbpg_autopublisher/
	cp -r ../../example/*conf stage/data/ddbpg_autopublisher/example/
	cp -r systemd/ddbpg_autopublisher.service stage/etc/systemd/system/ddbpg_autopublisher.service
	cp control stage/DEBIAN/	
	cp preinst stage/DEBIAN/
	cp postinst stage/DEBIAN/
	chmod 555 stage/DEBIAN/preinst
	chmod 555 stage/DEBIAN/postinst

package:
	dpkg-deb --build stage

clean:
	rm -rf ./stage
	rm -f *.deb
