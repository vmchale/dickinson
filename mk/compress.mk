# sak: https://hub.darcs.net/vmchale/sak

%.zst: %
	sak compress $< $@ --best

%.gz: %
	gzip -k -f $< --best

%.lz: %
	sak compress $< $@ --best

%.bz2: %
	sak compress $< $@ --best
