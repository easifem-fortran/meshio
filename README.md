# Converting mesh to EASIFEM format

`meshio`

To create `Domain` object in EASIFEM we need mesh. `meshio` is a command line application for converting mesh to EASIFEM mesh format.

EASIFEM mesh is a HDF5File. 

Currenty this app can convert Gmsh's `MSH4` format to EASIFEM format. 

## Installation 

### Method 1: bash script

Just run following command

```bash 
git clone https://github.com/easifem-fortran/meshio.git &&\
cd meshio &&\
bash install.sh
```


Note-1: You can specify the install directory by defining environment variable called `$EASIFEM_APP`. Then `meshio` will be installed at `$EASIFEM_APP/bin`. 

Note-2: If you have setup easifem on your system then `$EASIFEM_APP` will be already defined, which you can check by using `echo $EASIFEM_APP`


### CMake 

```bash 
git clone https://github.com/easifem-fortran/meshio.git &&\
cd meshio &&\
cmake -G "Ninja" -B build &&\
cmake --build build
```

In this case the app will be installed at `build/meshio`
