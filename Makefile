
SHELL:=/bin/bash

build:
	lein compile | grep -v 'at clojure.'

test:
	lein test | grep -v 'at clojure.' | grep -v '^    clojure\.'

run:
	lein run $(ARGS)

reflections:
	lein clean
	lein compile | sed 's|Reflection warning, ||' | sed "s| can't be resolved\\.||"

todo:
	grep 'TODO' -nR */ || true
	grep 'FIXME' -nR */ || true
	grep 'XXX' -nR */ || true

clean: pkg-clean

PROJNAME := mccormack_t_HW6
PACKAGE_FILE := "$(PROJNAME).tar.gz"
PKGDIR := "pkg/$(PROJNAME)"

pkg: pkg-clean
	mkdir -p "$(PKGDIR)/"
	cp README-CS4300 "$(PKGDIR)/README.txt"
	cp -r src/ "$(PKGDIR)/src"
	cp -r test/ "$(PKGDIR)/test"
	cp project.clj run.sh Makefile "$(PKGDIR)/"
	find ./pkg -name '*~' -delete
	tar -czf "$(PACKAGE_FILE)" --directory pkg/ "$(PROJNAME)/"

pkg-clean:
	rm -f "$(PROJNAME).tar.gz"
	rm -rf pkg
	lein clean

CCIS_MACHINE := timepilot.ccs.neu.edu
PRIVATE_DIR := ~/private
DEPLOY_DIR := $(PRIVATE_DIR)/CS4300-deploy

clean-deploy:
	ssh $(CCIS_MACHINE) 'rm -rf $(DEPLOY_DIR)'

deploy-ready: clean-deploy
	ssh $(CCIS_MACHINE) 'ls -ld $(PRIVATE_DIR)/ | grep drwx------ &>/dev/null'
	ssh $(CCIS_MACHINE) 'mkdir -p $(DEPLOY_DIR)'

deploy: pkg deploy-ready
	scp $(PACKAGE_FILE) deploy-test.sh '$(CCIS_MACHINE):$(DEPLOY_DIR)/'
	ssh $(CCIS_MACHINE) 'cd $(DEPLOY_DIR) && tar -xzf $(PACKAGE_FILE)'

test-deploy: deploy
	ssh -X $(CCIS_MACHINE) '$(DEPLOY_DIR)/deploy-test.sh'

.PHONY: pkg build test run todo deploy doc

.SILENT:

