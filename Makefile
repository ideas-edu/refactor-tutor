.PHONY: rpt
default: rpt

SRCDIR    = src
BINDIR    = .
DEPLOYDIR = D:/Software/XAMPP/cgi-bin
SCRIPTDIR = scripts
EXDIR     = exercises
OUTDIR    = out
<<<<<<< .mine
IDEASDIR  = ../../../Git/ideas/src
||||||| .r14533
IDEASDIR  = ../../../IdeasGit/ideas/src
=======
IDEASDIR  = ../../../../IdeasGit/ideas/src
>>>>>>> .r15366
MATHDIR   = ../../MathTutor/trunk/src
RM        = rm -rf

HS-SOURCES := $(shell find $(SRCDIR) -name "*.hs") $(shell find $(IDEASDIR) -name "*.hs")

GHC   = ghc
GHCI  = ghci
STRIP = echo
MKDIR = mkdir

IDEASSERVER = $(shell hostname | (grep -q science-vs105 && echo yes || echo no))

#ifeq ($(IDEASSERVER), yes)
DDB = -DDB
#endif

ifeq ($(IDEASSERVER), yes)
DEPLOYDIR = /var/www/cgi-bin
IDEASDIR  = ../../../ideas-git/src
STRIP     = strip
endif

GHCWARN  = -Wall -fwarn-tabs
GHCFLAGS = --make -O2 -i$(SRCDIR) -i$(IDEASDIR) -i$(MATHDIR) -odir $(OUTDIR) -hidir $(OUTDIR) $(DDB) $(GHCWARN)

rpt: $(BINDIR)/rpt.cgi

ghci:
	$(GHCI) -i$(SRCDIR) -i$(IDEASDIR) -odir $(OUTDIR) -hidir $(OUTDIR) $(GHCWARN)

clean:
	$(RM) $(BINDIR)/rpt.cgi
	$(RM) $(OUTDIR)

$(BINDIR)/rpt.cgi: $(HS-SOURCES)
	$(MKDIR) -p $(BINDIR) $(OUTDIR)
	$(GHC) $(GHCFLAGS) -o $@ src/Main.hs
	$(STRIP) $@
	cp $(SCRIPTDIR)/ref.txt $(DEPLOYDIR)/ref.txt
	cp $(BINDIR)/rpt.cgi $(DEPLOYDIR)/rpt.cgi
	cp -r $(EXDIR) $(DEPLOYDIR)
	
rpt-prof.cgi:
	$(GHC) $(GHCFLAGS) -prof -auto-all -o $@ src/Main.hs