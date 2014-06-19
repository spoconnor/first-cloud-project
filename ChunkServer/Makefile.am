## Process this file with automake to produce Makefile.in
## Created by Anjuta

SUBDIRS = src 

chunkserverdocdir = ${prefix}/doc/chunkserver
chunkserverdoc_DATA = \
	README\
	COPYING\
	AUTHORS\
	ChangeLog\
	INSTALL\
	NEWS


EXTRA_DIST = $(chunkserverdoc_DATA)


# Remove doc directory on uninstall
uninstall-local:
	-rm -r $(chunkserverdocdir)
