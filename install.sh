rm -rf ~/temp/tests/build
cmake -G "Ninja" -B ~/temp/tests/build
cmake --build ~/temp/tests/build
cp ~/temp/tests/build/msh2hdf ~/.easifem/easifem-app/bin/msh2hdf
