.PHONY:	help ? test deploy

LEIN = HTTP_CLIENT="curl --insecure -f -L -o" lein

all: tests

clean:
	rm -rf ./m2

jar:
	$(LEIN) jar

tests:
	$(LEIN) test

ci: clean tests

deploy:
	$(LEIN) deploy clojars

help: ?

?:
	@echo
	@echo "all ..................... Install all local dependencies and run tests"
	@echo "tests ................... Run all tests"
	@echo
	@echo "ci ...................... Target for CI build"
	@echo
	@echo "deploy .................. Deploy Kits on Clojar"
