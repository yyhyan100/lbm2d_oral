#MAKE_F = $(pwd)
SOU_F  = .
OBJDIR = .
EXEDIR = .
#
FFLAGS = -fdefault-real-8 #-C 
FC     = gfortran
#
MDL_FILE = vars.f90
TARGET = lbm2d.exe
#
SOURCES = $(wildcard $(SOU_F)/*.f90)
MOD_OBJ = $(patsubst %.f90, $(OBJDIR)/%.o, $(MDL_FILE))
OBJS = $(patsubst $(SOU_F)/%.f90, $(OBJDIR)/%.o, $(SOURCES))
#
$(TARGET): $(MOD_OBJ) $(OBJS)
	$(FC) $(FFLAGS) $(OBJS) -o $@
	@ echo
	@ echo ----------
	@ echo Executable file name : $@
	@ echo ----------
	@ echo


$(OBJS): Makefile $(SOU_F)/$(MDL_FILE)

$(OBJDIR)/%.o:$(SOU_F)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	@ echo Deleting *.o *.exe
	@ rm -f *.o
	@ rm -f *.obj
	@ rm -f *.mod
	@ rm -f $(TARGET)

cleanvtk:
	@ echo Deleting *.vtk
	@ rm -f *.vtk
