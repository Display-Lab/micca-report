# Micca Report Generator

Generate the pdfs from MICCA data.

## Development

### Autorun tests

From the project root directory, run the following in a terminal to watch for file system changes in the R or test code and autorun the tests when files are changed.
This won't pick up new files, so you'll have to terminate (ctrl+c) and restart it.

```sh
fd . R/ test/ | entr -c Rscript -e 'devtools::test()'
```
