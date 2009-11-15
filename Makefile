all:
	(cd deps;$(MAKE))
	(cd src;$(MAKE))

clean:              
	(cd src;$(MAKE) clean)

depclean:
	(cd deps;$(MAKE) clean)