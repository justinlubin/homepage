all:
	stack build && stack exec site rebuild

clean:
	stack exec site clean

publish:
	make clean && make all && git checkout gh-pages && ./copy.sh && \
		git add -A && git commit -m "Pull updates from 'main'" && git push && \
		git checkout master
