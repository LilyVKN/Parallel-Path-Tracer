FC = gfortran
MPIFC = mpifort

BUILD_DIR = ./build/obj
LIB_DIR = ./build/lib
SRC_FILES = $(wildcard ./*.f90)
OBJ_FILES = $(patsubst ./%.f90,$(BUILD_DIR)/%.o,$(SRC_FILES))

FFLAGS = -Wall -fdefault-real-8 -I$(BUILD_DIR) -J$(BUILD_DIR)

OBJ_COMPILE = $(FC) $(FFLAGS) -c -o $@ $^
LIB_COMPILE = ar cr $@ $^

default: $(LIB_DIR)/libmath.a

$(LIB_DIR)/libmath.a: $(BUILD_DIR)/vec.o $(OBJ_FILES)
	@mkdir -p $(@D)
	$(LIB_COMPILE)

clean:
	$(RM) $(BUILD_DIR)/*.o $(BUILD_DIR)/*.mod $(BUILD_DIR)/*.a

$(BUILD_DIR)/%.o: %.f90
	@mkdir -p $(@D)
	$(OBJ_COMPILE)