set -e # stop if anything fails
set -x # debug

rm -rf jinterface floaty-field src/main/resources/erlyberly/beam/*.beam

# first things first, compile erlyberl.erl
erlc +debug_info -o src/main/resources/erlyberly/beam/ src/main/resources/erlyberly/beam/erlyberly.erl

# install floaty field
git clone https://github.com/andytill/floaty-field.git
cd floaty-field
mvn install
cd ..

# install jinterface
git clone https://github.com/andytill/jinterface.git

cd jinterface
mvn install
cd ..

mvn clean install assembly:single