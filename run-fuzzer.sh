#!/bin/bash

# Get the assembly jar file name
JAR_FILE=$(sbt 'fuzz / assembly; fuzz / assembly / assemblyJarName' 2> /dev/null | grep -Eo '/.*\.jar$')

# Copy JSON test files to the fuzzer directory
cp -r fuzz/test/resources fuzz/target/scala-2.13

set -o xtrace
docker run -v $(pwd)/fuzz/target/scala-2.13:/fuzzing cifuzz/jazzer \
           --cp=$(basename $JAR_FILE) \
           --target_class=edu.rit.cs.dataunitylab.jsonoid.discovery.DiscoverSchemaFuzzer \
           -dict=resources/json.dict \
           resources
