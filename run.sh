set -e

echo "Installing dependencies..."
sudo apt update
sudo apt install -y build-essential cmake flex libfl-dev

BUILD_DIR_NAME="build"

echo "Creating build directory..."
mkdir -p "$BUILD_DIR_NAME"
cd "$BUILD_DIR_NAME"

echo "Configuring project with CMake..."
cmake .. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DUNIT_TESTING=ON

echo "Building project..."
make -j$(nproc)

echo "Running tests (ctest)..."
ctest --output-on-failure -V

