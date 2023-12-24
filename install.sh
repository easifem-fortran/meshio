#! /bin/bash
#

echo "    ███████╗ █████╗ ███████╗██╗███████╗███████╗███╗   ███╗"
echo "    ██╔════╝██╔══██╗██╔════╝██║██╔════╝██╔════╝████╗ ████║"
echo "    █████╗  ███████║███████╗██║█████╗  █████╗  ██╔████╔██║"
echo "    ██╔══╝  ██╔══██║╚════██║██║██╔══╝  ██╔══╝  ██║╚██╔╝██║"
echo "    ███████╗██║  ██║███████║██║██║     ███████╗██║ ╚═╝ ██║"
echo "    ╚══════╝╚═╝  ╚═╝╚══════╝╚═╝╚═╝     ╚══════╝╚═╝     ╚═╝"
echo "      EASIFEM \n"
echo "      Expandable And Scalable Infrastructure for Finite Element Methods \n"
echo "      (c) Vikas Sharma, vikas.easifem.com \n"
echo "      https://www.easifem.com \n"
echo "      https://github.com/easifem-fortran \n"
echo "      "


if [ -z "${EASIFEM_APP}" ]; then
  echo "meshio will be installed at ./bin/meshio"
  EASIFEM_APP="./"
else
  echo "meshio will be installed at ${EASIFEM_APP}/bin/meshio"
fi

build_dir=/tmp/easifem/meshio/build
rm -rf ${build_dir}
cmake -G "Ninja" -B ${build_dir}
cmake --build ${build_dir}
mv ${build_dir}/meshio ${EASIFEM_APP}/bin/meshio

echo "meshio is build at ${build_dir}"
echo "meshio is installed at ${EASIFEM_APP}/bin/meshio"
echo ""
echo "try following commands:"
echo "which meshio"
echo "meshio --help"
echo "meshio --version"
