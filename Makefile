.PHONY:	help ? test deploy

all: tests

jar:
	lein jar

tests:
	lein test

ci: tests

deploy:
	lein deploy clojars

help: ?

?:
	@echo
	@echo "all ..................... Install all local dependencies and run tests"
	@echo "tests ................... Run all tests"
	@echo
	@echo "ci ...................... Target for CI build"
	@echo
	@echo "deploy .................. Deploy Kits on Clojar"
