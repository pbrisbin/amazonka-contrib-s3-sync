# Amazonka S3 Sync

**Work in progress**

Haskell library for `aws s3 sync`-like functionality.

## Usage

## Options

| Options | Implemented? | Details |
| --- | --- | --- |
| `dryRun` | :x: | |
| `include`| :x: | |
| `exclude`| :x: | |
| `acl`| :x: | |
| `followSymlinks`| :x: | |
| `noGuessMimeType`| :x: | |
| `sse*`| :x: | |
| `storageClass`| :x: | |
| `grants`| :x: | |
| `websiteRedirect`| :x: | |
| `contentType`| :x: | |
| `cacheControl`| :x: | |
| `contentDisposition`| :x: | |
| `contentEncoding`| :x: | |
| `contentLanguage`| :x: | |
| `expires`| :x: | |
| `sourceRegion`| :x: | |
| `noProgress`| :x: | |
| `pageSize`| :x: | |
| `ignoreGlacierWarnings`| :x: | |
| `forceGlacierTransfers`| :x: | |
| `requestPayer`| :x: | |
| `metadata`| :x: | |
| `sizeOnly`| :x: | |
| `exactTimestamps`| :x: | |
| `delete`| :x: | |
| `endpointUrl`| :x: | |
| `noVerifySsl`| :x: | |
| `noPaginate`| :x: | |
| `noSignRequest`| :x: | |
| `caBundle`| :x: | |

## Design

```
sync(state, options, directory, bucket)
  {
    keys(bucket) | \key ->
      path = as-local(directory, key)
      state.seen.append(path)

      if fs.exists(path) and should-upload?(path, key)
        emit s3.put(path, key)
      else
        emit s3.delete(key)

    contents(directory)
      | filter (\path -> !state.seen?(path))
      | \path -> emit s3.put(path, to-key(bucket, path)
  }
  | decide-actions(options)
  | execute-actions(options)
```
