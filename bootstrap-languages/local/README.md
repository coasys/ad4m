# Local bootstrap languages

Plain-JS system languages that need no Holochain: agent, file-storage,
language, link, neighbourhood and perspective languages. They store data
through the `ad4m:host` KV (`storageGet` / `storagePut`), which belongs to one
executor. Use them with `ad4m-executor run --run-holochain false` for local,
standalone, Docker and test setups.

`node generate-seed.mjs . dist` writes a bootstrap seed that uses them
(`dist/docker_seed.json`), the language bundles under their addresses
(`dist/languages/<address>/bundle.js`), and a pre-filled KV file for the
language-language.

## Shared mode: `storagePath`

`language-language.js` and `neighbourhood-language.js` accept one language
setting, `storagePath`. Put it in `<data>/ad4m/languages/<address>/settings.json`
before the executor loads the language:

```json
{ "storagePath": "/abs/path/inside/the/executor/working/directory" }
```

With it, the language-language stores `meta-<address>.json` and
`bundle-<address>.js` in that directory, and the neighbourhood store stores
`neighbourhood-<address>.json`. They read and write through the optional File
I/O extension (`readStorageFile` / `writeStorageFile`). Every executor that
points at the same directory sees the languages and neighbourhoods the others
published. Without the setting they use the KV and see only their own.

The executor lets a system language access files in its own storage directory
and in the executor's working directory, nowhere else, so the directory must lie
inside the working directory. It must also exist already.

The integration tests (`tests/js`) use shared mode for every executor: see
`tests/js/utils/sharedStores.ts`.
