<img src="./doc/filehub-chan.png" alt="filehub chan" style="display:block; width:240px; margin:auto;" />

# Filehub

Filehub is a simple, no-frills, web-based, self-hosted file browser. It provides a unified interface for managing files across multiple backends simultaneously—for example, allowing you to browse and operate on files from both S3 and the local filesystem as if they were in the same place.


![demo1](./doc/filehub-demo0.png)

<div style="display: flex; gap: 10px;">
  <img src="./doc/filehub-demo1.png" width="390"/>
  <img src="./doc/filehub-demo2.png" width="390"/>
</div>

<div style="display: flex; gap: 10px;">
  <img src="./doc/filehub-demo4.png" width="390"/>
  <img src="./doc/filehub-demo3.png" width="390"/>
</div>


<div style="display: flex; gap: 10px;">
  <img src="./doc/filehub-demo5.png" width="200"/>
  <img src="./doc/filehub-demo6.png" width="200"/>
  <img src="./doc/filehub-demo7.png" width="200"/>
</div>



### Download
You can download binary from the release.


### Setup
To start an instance, run the following command:

```
filehub --port 8080 --theme dark --fs $HOME
```


Now you can access the local `$HOME` directory on `http://localhost:8080`.


To add a S3 target, you can use the `--s3 <bucket>` option

```
filehub --port 8080 --theme dark --s3 book --s3 files
```

Filehub will search for the AWS credentials from the standard locations. Note: if you use a third party S3 provider, you can provide the endpoint via `AWS_ENDPOINT_URL`.


### Features
- File manager style interface
- Multiple storage backend (curently supports posix file system and S3)
- Copy/paste files across storage backends.
- Image viewer.
- PDF, Video, audio supports.
- Sorting
- Read only mode


### Nix
There is a nix module defined in `/nix` folder.
