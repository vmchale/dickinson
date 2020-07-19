# sak: https://hub.darcs.net/vmchale/sak

%.zst: %
	sak compress $< $@ --best
	chmod +x $@

%.gz: %
	gzip -k -f $< --best

%.lz: %
	sak compress $< $@ --best
	chmod +x $@

%.bz2: %
	sak compress $< $@ --best
	chmod +x $@
