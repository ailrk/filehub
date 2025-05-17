# Filehub


Filehub is a simple, no frills, web based, self-hostable file browser.

![demo1](./doc/filehub-demo4.png)

<div style="display: flex; gap: 10px;">
  <img src="./doc/filehub-demo0.png" width="200"/>
  <img src="./doc/filehub-demo1.png" width="200"/>
  <img src="./doc/filehub-demo2.png" width="200"/>
  <img src="./doc/filehub-demo3.png" width="200"/>
</div>


### Download
You can download binary from the release.


### Setup
To start an instance, run the following command:

```
filehub --port 8080 --root $HOME --theme dark1
```

Now you can access `$HOME` directory on `http://localhost:8080`.


### Nix
There is a nix module defined in `/nix` folder.
