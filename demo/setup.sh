#!/bin/bash

set -e # exit on error

usage()
{
  echo "Fiats Demonstration Applications Setup Script"
  echo ""
  echo "USAGE:"
  echo "./setup.sh [--help|-h] | [-p|--prefix=PREFIX]"
  echo ""
  echo " --help             Display this help text"
  echo " --prefix=PREFIX    Install binary in 'PREFIX/bin'"
  echo "                    Default prefix='\$HOME/.local/bin'"
  echo ""
}

PREFIX="$HOME/.local"

while [ "$1" != "" ]; do
  PARAM=$(echo "$1" | awk -F= '{print $1}')
  VALUE=$(echo "$1" | awk -F= '{print $2}')
  case $PARAM in
    -h | --help)
      usage
      exit
      ;;
    -p | --prefix)
      PREFIX=$VALUE
      ;;
    *)
      echo "ERROR: unknown parameter \"$PARAM\""
      usage
      exit 1
      ;;
  esac
  shift
done

set -u # error on use of undefined variable

FPM_FC=${FPM_FC:-"flang-new"}
FPM_CC=${FPM_CC:-"clang"}

if [ $(uname) = "Darwin" ]; then
  if command -v brew ; then
    brew install netcdf netcdf-fortran pkg-config coreutils # coreutils supports `realpath` below
    NETCDF_LIB_PATH="`brew --prefix netcdf`/lib"
    HDF5_LIB_PATH="`brew --prefix hdf5`/lib"
    NETCDFF_LIB_PATH="`brew --prefix netcdf-fortran`/lib"
    fpm_cc_version=$($FPM_CC --version)
    if [[ $fpm_cc_version = Apple* ]]; then
      echo "$FPM_CC appears to be an Apple compiler.  Please set FPM_CC to the location of LLVM clang."
      exit 1
    fi
    if [[ -z ${LC_RPATH:-} ]]; then
      printf "Please set LC_RPATH=\$DYLD_LIBRARY_PATH path and restart this script.\n\n"
      exit 1
    fi
  else
    cat <<'EOF'

      Command 'brew' not found. On macOS, this script uses Homebrew (https://brew.sh) to 
      install the prerequisite packages netcdf, netcdf-fortran, pkg-config, and coreutils.
      Please install Homebrew and restart this script."
EOF
  fi
elif [ $(uname) = "Linux" ]; then
  if [[ -z ${HDF5_LIB_PATH:-}    ]]; then 
    printf "Please set HDF5_LIB_PATH to the HDF5 library path and restart this script.\n\n"
    exit 1
  fi
  if [[ -z ${NETCDF_LIB_PATH:-}  ]]; then
    printf "Please set NETCDF_LIB_PATH to the NetCDF library path and restart this script.\n\n"
     exit 1
  fi
  if [[ -z ${NETCDFF_LIB_PATH:-} ]]; then
    printf "Please set NETCDFF_LIB_PATH to the NetCDF-Fortran library path and restart this script.\n\n"
    exit 1
  fi
  if [[ -z ${LC_RPATH:-} ]]; then
    printf "Please set LC_RPATH=\$LD_LIBRARY_PATH path and restart this script.\n\n"
    exit 1
  fi
fi


FPM_LD_FLAG=" -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH -L$NETCDFF_LIB_PATH -rpath $LC_RPATH"

PREFIX=`realpath $PREFIX`

fpm_fc_version=$($FPM_FC --version)
if [[ $fpm_fc_version = flang* ]]; then
  if [[ $fpm_fc_version = *19* ]]; then
    FPM_FLAG="-mmlir -allow-assumed-rank -O3 -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH"
  else
    FPM_FLAG="-O3 -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH"
  fi
elif [[ $fpm_fc_version = GNU* ]]; then
  echo
  echo "$FPM_FC appears to be gfortran, which is currently unsupported due to compiler bugs for parameterized derived types."
  echo
  exit 1
  FPM_FLAG="-fcoarray=single -O3 -fallow-argument-mismatch -ffree-line-length-none -L$NETCDF_LIB_PATH -L$HDF5_LIB_PATH"
  FPM_RUNNER="cafrun -n 1"
  FPM_CC="mpicc"
else
  FPM_FLAG=""
fi

mkdir -p build

CI=${CI:-"false"} # GitHub Actions workflows set CI=true

if [ $CI = true ]; then
  PKG_CONFIG_PATH=`realpath ./build/pkgconfig`
  echo "---------------"
  echo "PKG_CONFIG_PATH=$PKG_CONFIG_PATH"
  echo "---------------"
else
  PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig"
  if [ ! -d "$PKG_CONFIG_PATH" ]; then
    mkdir -p "$PKG_CONFIG_PATH"
  fi
  PKG_CONFIG_PATH=`realpath "$PKG_CONFIG_PATH"`
fi

FIATS_VERSION=$(grep version ../fpm.toml | grep -o '".*"' - | sed 's/"//g')

FIATS_PC="$PKG_CONFIG_PATH/fiats.pc"
echo "FIATS_FPM_CC=\"$FPM_CC\""                   >  $FIATS_PC
echo "FIATS_FPM_FC=\"$FPM_FC\""                   >> $FIATS_PC
if [[ ! -z ${FPM_RUNNER:-} ]];  then
  echo "FIATS_FPM_RUNNER=\"$FPM_RUNNER\""         >> $FIATS_PC
fi
echo "FIATS_FPM_LD_FLAG=\"$FPM_LD_FLAG\""         >> $FIATS_PC
echo "FIATS_FPM_FLAG=\"$FPM_FLAG\""               >> $FIATS_PC
echo "Name: fiats"                                >> $FIATS_PC
echo "Description: Fiats"                         >> $FIATS_PC
echo "URL: https://github.com/berkeleylab/fiats " >> $FIATS_PC
echo "Version: $FIATS_VERSION"                    >> $FIATS_PC

if [ $CI = true ]; then
  echo "---------------"
  echo "cat \$FIATS_PC"
  cat $FIATS_PC
  echo "---------------"
fi

export PKG_CONFIG_PATH
cp src/run-fpm.sh-header build/run-fpm.sh
RUN_FPM_SH="`realpath ./build/run-fpm.sh`"
echo "`which fpm` \$fpm_arguments \\" >>  $RUN_FPM_SH
echo "--profile release \\" >> $RUN_FPM_SH
echo "--c-compiler \"`pkg-config fiats --variable=FIATS_FPM_CC`\" \\"     >> $RUN_FPM_SH
echo "--compiler \"`pkg-config fiats --variable=FIATS_FPM_FC`\" \\"       >> $RUN_FPM_SH
if [[ ! -z ${FPM_RUNNER:-} ]];  then
  echo "--runner \"`pkg-config fiats --variable=FIATS_FPM_RUNNER`\" \\"   >> $RUN_FPM_SH
fi
echo "--flag \"-cpp `pkg-config fiats --variable=FIATS_FPM_FLAG`\" \\"    >> $RUN_FPM_SH
echo "--link-flag \"`pkg-config fiats --variable=FIATS_FPM_LD_FLAG`\" \\" >> $RUN_FPM_SH
echo "\$program_arguments"                                                           >> $RUN_FPM_SH
chmod u+x $RUN_FPM_SH
if [ $CI = true ]; then
  echo "---------------"
  echo "cat $RUN_FPM_SH"
  cat $RUN_FPM_SH
  echo "---------------"
fi

$RUN_FPM_SH build

echo ""
echo "____________________ The fiats demo apps build succeeded! _______________________"
echo ""
echo "Run the following command to see a list of available apps:"
echo ""
echo "./build/run-fpm.sh run"
echo ""
echo "Append a space followed by an app's name to see basic app usage information."
