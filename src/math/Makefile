# default compilers for FORTRAN files
FC = gfortran
MPIFC = mpifort

# directory information
CURRENT_DIR = $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
BUILD_DIR = $(CURRENT_DIR)/build/obj
LIB_DIR = $(CURRENT_DIR)/build/lib
BIN_DIR = $(CURRENT_DIR)/build/bin

# macros for retrieving source files and their outputs
SRC_FILES = $(wildcard ./*.f90)
OBJ_FILES = $(patsubst ./%.f90,$(BUILD_DIR)/%.o,$(SRC_FILES))

FFLAGS = -Wall -fdefault-real-8 -I$(BUILD_DIR) -J$(BUILD_DIR)

# compiling macros
OBJ_COMPILE = $(FC) $(FFLAGS) -c -o $@ $^
LIB_COMPILE = ar cr $@ $^

default: $(LIB_DIR)/libmath.a

# library =====================================================================
$(LIB_DIR)/libmath.a: $(BUILD_DIR)/vec.o $(OBJ_FILES)
	@mkdir -p $(@D)
	$(LIB_COMPILE)

# cleanup functions ===========================================================
clean:
	$(RM) $(BUILD_DIR)/*.o $(BUILD_DIR)/*.mod $(BUILD_DIR)/*.a

# default compile =============================================================
$(BUILD_DIR)/%.o: %.f90
	@mkdir -p $(@D)
	$(OBJ_COMPILE)