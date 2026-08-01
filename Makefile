# Portage-ng makefile

## ----------------------------------------------------------------------
## You need to have a recent version of SWI-Prolog installed. Preferably 
## a development version (version 10.0.0 or higher). This Makefile will help
## you create a 'portage-ng' binary that can be installed in /usr/local
##
## See manpage for more information on how to use portage-ng.
## ----------------------------------------------------------------------


SUDO=sudo
TARGET=portage-ng

BUILDDIR=$(shell pwd)

help:     ## Show this help.
	  @sed -ne '/@sed/!s/## //p' $(MAKEFILE_LIST)

check:    ## Verify development environment (SWI-Prolog, shell aliases).
	  @ok=true; \
	  if ! command -v swipl >/dev/null 2>&1; then \
	    echo "ERROR: swipl not found in PATH."; \
	    echo "  Install SWI-Prolog >= 10.0.0: https://www.swi-prolog.org/download/devel"; \
	    ok=false; \
	  else \
	    echo "OK: swipl found ($$(swipl --version 2>&1 | head -1))"; \
	  fi; \
	  if ! command -v portage-ng-dev >/dev/null 2>&1; then \
	    echo "WARNING: portage-ng-dev not found as alias."; \
	    echo "  Add the following to your ~/.zshrc or ~/.bash_profile:"; \
	    echo ""; \
	    echo "  alias portage-ng-dev=\"swipl -O \\\\"; \
	    echo "    --stack-limit=32G \\\\"; \
	    echo "    -f $(BUILDDIR)/portage-ng.pl \\\\"; \
	    echo "    -p portage=$(BUILDDIR) \\\\"; \
	    echo "    -Dverbose_autoload=false \\\\"; \
	    echo "    -g main --\""; \
	    echo ""; \
	    echo "  Then reload your shell: source ~/.zshrc or ~/.bash_profile"; \
	  else \
	    echo "OK: portage-ng-dev found"; \
	  fi; \
	  if [ "$$ok" = false ]; then exit 1; fi

## all:      build & install
all:	  check build install 

build:	  ## Build the application.
	  swipl -O --stack_limit=32G -o $(TARGET)  -q -f portage-ng.pl -p portage=${BUILDDIR} -g main --stand_alone=true -c portage-ng.pl

install:  ## Install the application.
	  $(SUDO) cp $(TARGET) /usr/local/bin

test:     ## Run PLUnit tests.
	  printf '%s\n' \
	    "load_files(portage('Source/Test/unittest'), [if(true)])." \
	    "run_tests." \
	    "halt." | \
	    ./Source/Application/Wrapper/portage-ng-dev --mode standalone --shell

test-overlay: ## Run overlay regression tests (requires loaded overlay repository).
	  printf '%s\n' \
	    "test:run(cases)." \
	    "halt." | \
	    ./Source/Application/Wrapper/portage-ng-dev --mode standalone --shell

test-profile-mask-golden: ## Validate profile package.mask against the golden snapshot (needs kb.qlf + profile.qlf).
	  printf '%s\n' \
	    "load_files(portage('Source/Test/unittest'), [if(true)])." \
	    "profile_mask_golden_main." \
	    "halt." | \
	    ./Source/Application/Wrapper/portage-ng-dev --mode standalone --shell

test-profile-mask-golden-update: ## Regenerate the profile package.mask golden snapshot in unittest.pl.
	  printf '%s\n' \
	    "load_files(portage('Source/Test/unittest'), [if(true)])." \
	    "profile_mask_golden_update." \
	    "halt." | \
	    ./Source/Application/Wrapper/portage-ng-dev --mode standalone --shell

clean:    ## Remove the built binary.
	  rm -f $(TARGET)

CERTDIR=Certificates
HOST?=$(shell hostname)

certs:    ## Generate local CA + per-host client/server TLS certs (for --mode client/server). Usage: make certs HOST=mac-pro.local
	  sh $(CERTDIR)/Scripts/generate.sh $(HOST)
	  @if [ ! -f $(CERTDIR)/passwordfile ]; then \
	    echo "NOTE: $(CERTDIR)/passwordfile missing."; \
	    echo "  Set Source/Config/Private/passwords.pl then: make passwordfile"; \
	  fi

passwordfile: ## Derive Certificates/passwordfile from Source/Config/Private/passwords.pl
	  sh $(CERTDIR)/Scripts/digestpassword.sh

certs-check: ## Check TLS certificate expiry status for all hosts.
	  @sh $(CERTDIR)/Scripts/generate.sh --check

certs-renew: ## Renew expired/expiring TLS certificates for all hosts.
	  sh $(CERTDIR)/Scripts/generate.sh --renew

.PHONY: help check all build install test test-overlay test-profile-mask-golden test-profile-mask-golden-update clean certs passwordfile certs-check certs-renew
