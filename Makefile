EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch

SRCS   = rere.el
TESTS  = test/rere-test.el
ELCS   = $(SRCS:.el=.elc)
ELPA_DIR ?= $(HOME)/.config/emacs/elpa
ELPA_DIRS = $(wildcard $(ELPA_DIR)/*)
DEP_FLAGS = $(patsubst %,-L %,$(ELPA_DIRS))

LOAD_PATH = -L . $(DEP_FLAGS)

.PHONY: all test compile check-style clean

all: check-style compile test

compile: $(ELCS)

%.elc: %.el
	$(BATCH) $(LOAD_PATH) \
	  -f batch-byte-compile $<

test:
	$(BATCH) $(LOAD_PATH) \
	  -l ert -l $(TESTS) \
	  -f ert-run-tests-batch-and-exit

check-style:
	@echo "Checking line length..."
	@awk 'length > 80 { \
	  print FILENAME ":" FNR ": " $$0; found=1 \
	} END { if (found) exit 1 }' \
	  $(SRCS) $(TESTS) README.org 2>/dev/null; \
	  echo "Style check passed."

clean:
	rm -f $(ELCS)
