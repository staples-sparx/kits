.PHONY:	help ? test deploy

S3_USERNAME = $(shell grep access_key ~/.s3cfg | head -n1 | awk -F ' = ' '{print $$2 }')
S3_PASSPHRASE = $(shell grep secret_key ~/.s3cfg | head -n1 | awk -F ' = ' '{print $$2}')
LEIN_ENV = S3_USERNAME="$(S3_USERNAME)" S3_PASSPHRASE="$(S3_PASSPHRASE)" HTTP_CLIENT="curl --insecure -f -L -o"

LEIN = $(LEIN_ENV) lein

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

s3-deploy:
	$(LEIN) deploy s3-releases

help: ?

?:
	@echo
	@echo "all ..................... Install all local dependencies and run tests"
	@echo "tests ................... Run all tests"
	@echo
	@echo "ci ...................... Target for CI build"
	@echo
	@echo "deploy .................. Deploy Kits artifacts on Clojars.org (requires gpg signing)"
