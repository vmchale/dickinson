%.zst: %
	sak compress $< $@ --best

%.gz: %
	sak compress $< $@ --best

%.lz: %
	sak compress $< $@ --best

%.bz2: %
	sak compress $< $@ --best
