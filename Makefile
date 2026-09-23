EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch --eval "(setq load-prefer-newer t)"

SRCS   = rere.el
TESTS  = test/rere-test.el
BENCH  = test/rere-bench.el
ELCS   = $(SRCS:.el=.elc)
ELPA_DIR  ?= $(HOME)/.config/emacs/elpa
ELPA_DIRS  = $(wildcard $(ELPA_DIR)/*) \
             $(wildcard $(HOME)/.emacs.d/elpa/*) \
             $(wildcard $(HOME)/.config/emacs/elpa/*)
DEP_FLAGS  = $(patsubst %,-L %,$(sort $(ELPA_DIRS)))

LOAD_PATH = -L . $(DEP_FLAGS)

.PHONY: all test bench compile check-style clean

all: check-style compile test

compile: $(ELCS)

%.elc: %.el
	$(BATCH) $(LOAD_PATH) \
	  -f batch-byte-compile $<

test:
	$(BATCH) $(LOAD_PATH) \
	  -l ert -l $(TESTS) \
	  -f ert-run-tests-batch-and-exit

bench: compile
	$(BATCH) $(LOAD_PATH) \
	  -l $(BENCH) \
	  -f rere-bench-run

check-style:
	@echo "Checking line length..."
	@awk 'length > 80 { \
	  print FILENAME ":" FNR ": " $$0; found=1 \
	} END { if (found) exit 1 }' \
	  $(SRCS) $(TESTS) $(BENCH) && \
	  echo "Style check passed."

clean:
	rm -f $(ELCS)
