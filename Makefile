all:
	rm -f search/ebin/*; \
	erl -make; \
	cp mochiweb/ebin/* mochiweb_xpath/ebin/* search/ebin/; \
	erl -sname search -pa search/ebin/ -mnesia dir '"search/data/Mnesia.search"'
clean:
	rm -rfv search/ebin/*
