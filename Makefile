LISP = sbcl

build:
	$(LISP) \
		--load clnote.asd \
		--eval "(ql:quickload :clnote)" \
		--eval "(asdf:make :clnote)" \
		--eval "(quit)"
