VERSION=0.1.0
COMPONENT_INTERNAL=ddbpg_autopublisher
COMPONENT=ddbpg_autopublisher
DEPS="erlang" "coreutils" "sudo"

include ../../_build/default/lib/fifo_utils/priv/pkg.mk

.PHONY: prepare

prepare:
	-rm -r $(STAGE_DIR)/$(COMPONENT)
	cp -r ../../_build/prod/rel/$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(COMPONENT)

package: prepare $(FILE).tgz

clean: clean-pkg
	-rm *.tgz

