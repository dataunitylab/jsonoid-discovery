const fs = require('fs');
const readline = require('readline');

const Ajv = require('ajv/dist/2019')
const addFormats = require('ajv-formats');

const ajv = new Ajv({
  strict: false
});
addFormats(ajv);

const packageSchema = JSON.parse(fs.readFileSync(process.argv[2]));
const validate = ajv.compile(packageSchema);

(async () => {
  // Read lines from stdin
  const fileStream = fs.createReadStream('/dev/stdin');
  const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity
  });

  // Parse and try to validate each line
  for await (const line of rl) {
    const json = JSON.parse(line);
    const valid = validate(json);
    if (!valid) {
      console.log(JSON.stringify(validate.errors));
    }
  }
})();
