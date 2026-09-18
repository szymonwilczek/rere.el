EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch

SRCS   = rere.el
TESTS  = test/rere-test.el
ELCS   = $(SRCS:.el=.elc)

.PHONY: all test compile check-style clean

all: check-style compile test

compile: $(ELCS)

%.elc: %.el
	$(BATCH) -L . -f batch-byte-compile $<

test:
	$(BATCH) -L . -l ert -l $(TESTS) \
	  -f ert-run-tests-batch-and-exit

check-style:
	@echo "Checking line length (max 80 columns)..."
	@awk 'length > 80 { \
	  print FILENAME ":" FNR ": " $$0; found=1 \
	} END { if (found) exit 1 }' \
	  $(SRCS) $(TESTS) README.org 2>/dev/null; \
	  echo "Style check passed."

clean:
	rm -f $(ELCS)
