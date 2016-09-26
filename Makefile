all:
	rm -f search/ebin/*; \
	erl -make; \
	cp mochiweb/ebin/* mochiweb_xpath/ebin/* search/ebin/; \
	erl -sname search -pa search/ebin/
clean:
	rm -rfv search/ebin/*
