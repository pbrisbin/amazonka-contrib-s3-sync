> [!NOTE]
> All of my GitHub repositories have been **archived** and will be migrated to
> Codeberg as I next work on them. This repository either now lives, or will
> live, at:
>
> https://codeberg.org/pbrisbin/amazonka-contrib-s3-sync
>
> If you need to report an Issue or raise a PR, and this migration hasn't
> happened yet, send an email to me@pbrisbin.com.

# Amazonka S3 Sync

Haskell library for `aws s3 sync`-like functionality.

## Usage

TODO

## Roadmap

This **highly work-in-progress**. The following is a list of what we've got so
far.

### Logic

| From/To | Local | Remote             |
| ------- | ----- | ------------------ |
| Local   | :x:   | :heavy_check_mark: |
| Remote  | :x:   | :x:                |

### Actions

| Action               | Implemented? |
| -------------------- | ------------ |
| create/update file   | :x:          |
| delete file          | :x:          |
| create/update object | :x:          |
| delete object        | :x:          |

### Options

| Option                  | Implemented?       | Details |
| ----------------------- | ------------------ | ------- |
| `dryRun`                | :heavy_check_mark: |         |
| `include`               | :heavy_check_mark: |         |
| `exclude`               | :heavy_check_mark: |         |
| `acl`                   | :x:                |         |
| `followSymlinks`        | :x:                |         |
| `noGuessMimeType`       | :x:                |         |
| `sse*`                  | :x:                |         |
| `storageClass`          | :x:                |         |
| `grants`                | :x:                |         |
| `websiteRedirect`       | :x:                |         |
| `contentType`           | :x:                |         |
| `cacheControl`          | :x:                |         |
| `contentDisposition`    | :x:                |         |
| `contentEncoding`       | :x:                |         |
| `contentLanguage`       | :x:                |         |
| `expires`               | :x:                |         |
| `sourceRegion`          | :x:                |         |
| `noProgress`            | :x:                |         |
| `pageSize`              | :x:                |         |
| `ignoreGlacierWarnings` | :x:                |         |
| `forceGlacierTransfers` | :x:                |         |
| `requestPayer`          | :x:                |         |
| `metadata`              | :x:                |         |
| `sizeOnly`              | :heavy_check_mark: |         |
| `exactTimestamps`       | :x:                |         |
| `delete`                | :heavy_check_mark: |         |
| `endpointUrl`           | :x:                |         |
| `noVerifySsl`           | :x:                |         |
| `noPaginate`            | :x:                |         |
| `noSignRequest`         | :x:                |         |
| `caBundle`              | :x:                |         |

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
