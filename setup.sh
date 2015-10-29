set -e # stop if anything fails
set -x # debug

rm -rf jinterface floaty-field src/main/resources/erlyberly/beam/*.beam

# first things first, compile erlyberl.erl
erlc +debug_info -o src/main/resources/erlyberly/beam/ src/main/resources/erlyberly/beam/erlyberly.erl

# install floaty field
git clone https://github.com/andytill/floaty-field.git
cp -r .mvn floaty-field/
cp mvnw floaty-field/
cd floaty-field
./mvnw install
cd ..

# install jinterface
git clone https://github.com/andytill/jinterface.git
cp -r .mvn jinterface/
cp mvnw jinterface/
cd jinterface
./mvnw install
cd ..

./mvnw clean install assembly:single