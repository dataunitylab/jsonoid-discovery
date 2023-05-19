#!/bin/bash

# Get the assembly jar file name
JAR_FILE=$(sbt 'fuzz / assembly; fuzz / assembly / assemblyJarName' 2> /dev/null | grep -Eo '/.*\.jar$')

# Copy test files to the temporary test directory
TMP_DIR=$(mktemp -d)
cp fuzz/test/resources/json.dict $TMP_DIR
cp -r src/test/resources  $TMP_DIR
cp $JAR_FILE $TMP_DIR

set -o xtrace
docker run -v $TMP_DIR:/fuzzing cifuzz/jazzer \
           --cp=$(basename $JAR_FILE) \
           --target_class=edu.rit.cs.dataunitylab.jsonoid.discovery.DiscoverSchemaFuzzer \
           -dict=json.dict \
           resources
