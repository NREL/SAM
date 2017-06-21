PHPDIR=/usr/include/php
SSCDIR=/Users/adobos/Projects/ssc/apiwrappers/php

sscphp.so: sscphp.c
	gcc -shared -O2 -fPIC -I$(PHPDIR) -I$(PHPDIR)/TSRM -I$(PHPDIR)/main -I$(PHPDIR)/ext -I$(PHPDIR)/Zend -o sscphp.so sscphp.c $(SSCDIR)/ssc.dylib

clean:
	rm sscphp.so
