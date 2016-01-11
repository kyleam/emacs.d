
LOAD_PATH ?=
BATCH = emacs -Q --batch $(LOAD_PATH)

LOADDEFS_FNAME = km-emacs-autoloads.el
LOADDEFS = lisp/$(LOADDEFS_FNAME)

ELS = $(shell find lisp -maxdepth 1 \
	-type f \( -name "*.el" -and ! -name "$(LOADDEFS_FNAME)" \) -print)

$(LOADDEFS): $(ELS)
	@$(BATCH) --eval "\
	(let* ((make-backup-files nil) \
	       (default-directory \"$(CURDIR)\") \
	       (generated-autoload-file (expand-file-name \"$(LOADDEFS)\"))) \
	 (update-directory-autoloads \"lisp/\"))"

%.elc: %.el
	@$(BATCH) -f batch-byte-compile $<
