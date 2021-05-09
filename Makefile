all: clean publish serve

EMACS = emacs-27.2
PYTHON = python3.9
USERNAME := $(shell id -un)
PORT := 8000
WWW := www

publish: publish.el
	$(EMACS) -u $(USERNAME) --batch --load publish.el --funcall org-publish-all

serve:
	$(PYTHON) -m http.server -d $(WWW) $(PORT)

clean:
	echo "Cleaning up.."
	rm -rvf *.elc
	rm -rvf www
	rm -rvf ~/.org-timestamps/*
	rm -rvf ~/.emacs.d/.cache/.org-timestamps
