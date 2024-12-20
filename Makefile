# InterSect Makefile

# ifort
#FF = ifort
#FFLAGS = -O3 -heap-arrays=1024
#FFLAGS = -g -check bounds -no-heap-arrays

# gfortran
FF = gfortran
FFLAGS = -O3 -march=native
#FFLAGS = -g -fbounds-check

#OPENMPFLAGS = -qopenmp
OPENMPFLAGS = -fopenmp
#OPENMPFLAGS =

BASENAME = insect

FFLAGS += $(OPENMPFLAGS)
EXE = $(BASENAME)$(OPENMPFLAGS)

objects = main.o reallocate.o bruteforce.o discosegment.o heapsort2.o \
	intersect.o readgmsh.o readmesh.o readdisco.o createsegment.o seconds.o \
	writevtu.o
# Makefile
all: $(objects)
	$(FF) $(FFLAGS) -o $(EXE) $(objects)
mod_reallocate.mod: reallocate.f90
	$(FF) $(FFLAGS) -c reallocate.f90
reallocate.o: reallocate.f90
	$(FF) $(FFLAGS) -c reallocate.f90
createsegment.o: createsegment.f90
	$(FF) $(FFLAGS) -c createsegment.f90
mod_bruteforce.mod: mod_reallocate.mod bruteforce.f90
	$(FF) $(FFLAGS) -c bruteforce.f90
bruteforce.o: mod_reallocate.mod bruteforce.f90
	$(FF) $(FFLAGS) -c bruteforce.f90
mod_writevtu.mod: writevtu.f90
	$(FF) $(FFLAGS) -c writevtu.f90
writevtu.o: writevtu.f90
	$(FF) $(FFLAGS) -c writevtu.f90
readgmsh.o: readgmsh.f90
	$(FF) $(FFLAGS) -c readgmsh.f90
main.o: mod_reallocate.mod mod_bruteforce.mod \
         mod_writevtu.mod main.f90
	$(FF) $(FFLAGS) -c main.f90
discosegment.o: discosegment.f90
	$(FF) $(FFLAGS) -c discosegment.f90
readmesh.o: readmesh.f90
	$(FF) $(FFLAGS) -c readmesh.f90
readdisco.o: readdisco.f90
	$(FF) $(FFLAGS) -c readdisco.f90
heapsort2.o: heapsort2.f90
	$(FF) $(FFLAGS) -c heapsort2.f90
intersect.o: intersect.f90
	$(FF) $(FFLAGS) -c intersect.f90
seconds.o: seconds.f90
	$(FF) $(FFLAGS) -c seconds.f90


# Cleaning everything
clean:
	rm -f mod_reallocate.mod mod_bruteforce.mod \
    mod_writevtu.mod $(EXE)
	rm -f $(objects)
# End
